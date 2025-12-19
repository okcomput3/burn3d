/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2025 Scott Moreau <oreaus@gmail.com>
 * Copyright (c) 2025 Andrew Pliatsikas <futurebytestore@gmail.com>
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 */

#include <wayfire/core.hpp>
#include <wayfire/opengl.hpp>
#include <wayfire/view-transform.hpp>
#include <wayfire/util/duration.hpp>
#include <wayfire/plugins/animate/animate.hpp>
static const char *burn_vert_source =
    R"(
#version 100

attribute highp vec2 position;
attribute highp vec2 uv_in;

varying highp vec2 uvpos;

void main() {

    gl_Position = vec4(position.xy, 0.0, 1.0);
    uvpos = uv_in;
}
)";

static const char *burn_frag_source =
    R"(
#version 100
@builtin_ext@
@builtin@
precision highp float;
varying highp vec2 uvpos;
uniform vec2 size;
uniform float progress;
uniform int direction;
uniform float flame_speed;
uniform float flame_width;
uniform float flame_height;
uniform vec4 flame_color;

// Simple hash for burn line noise
float hash1(float p) { return fract(sin(p * 127.1) * 43758.5453123); }
float hash2(vec2 p) { return fract(sin(dot(p, vec2(127.1, 311.7))) * 43758.5453123); }

// Smooth noise function (C1 continuous)
float smooth_hash(float p, float t) {
    float i = floor(p);
    float f = fract(p);
    f = f * f * (3.0 - 2.0 * f);
    return mix(hash2(vec2(i, floor(t))), hash2(vec2(i + 1.0, floor(t))), f);
}

// Derivative of smooth_hash
float smooth_hash_deriv(float p, float t, float scale) {
    float i = floor(p);
    float f = fract(p);
    float df = 6.0 * f * (1.0 - f);
    float h0 = hash2(vec2(i, floor(t)));
    float h1 = hash2(vec2(i + 1.0, floor(t)));
    return (h1 - h0) * df * scale;
}

// 1D smooth interpolation
float smooth_hash1(float p) {
    float i = floor(p);
    float f = fract(p);
    f = f * f * (3.0 - 2.0 * f);
    return mix(hash1(i), hash1(i + 1.0), f);
}

// 2D smooth noise with C2 continuity (quintic interpolation)
// From lecture: particles form discrete approximation to continuous material
float smooth_noise2D(vec2 p) {
    vec2 i = floor(p);
    vec2 f = fract(p);
    // Quintic: 6t^5 - 15t^4 + 10t^3 (C2 continuous - zero 1st and 2nd derivatives at boundaries)
    vec2 u = f * f * f * (f * (f * 6.0 - 15.0) + 10.0);
    
    float a = hash2(i + vec2(0.0, 0.0));
    float b = hash2(i + vec2(1.0, 0.0));
    float c = hash2(i + vec2(0.0, 1.0));
    float d = hash2(i + vec2(1.0, 1.0));
    
    return mix(mix(a, b, u.x), mix(c, d, u.x), u.y);
}

// Paper fiber density using FBM (Fractal Brownian Motion)
// From lecture: gases diffuse from dense to less dense regions
// Paper fibers create varying density - thin areas burn faster
float paper_fiber(vec2 uv, float t) {
    float fiber = 0.0;
    // Multiple octaves of noise (like Fourier series approximation)
    fiber += smooth_noise2D(uv * 15.0) * 0.5;           // Coarse fibers
    fiber += smooth_noise2D(uv * 35.0 + t * 0.01) * 0.3; // Medium detail
    fiber += smooth_noise2D(uv * 70.0) * 0.2;           // Fine grain
    return fiber;
}

// Turbulence function for realistic flame flickering
// From lecture: flames don't travel in straight lines
float turbulence(vec2 p, float t) {
    float turb = 0.0;
    float scale = 1.0;
    float amp = 1.0;
    for (int i = 0; i < 4; i++) {
        turb += smooth_noise2D(p * scale + vec2(0.0, t * 0.5)) * amp;
        scale *= 2.0;
        amp *= 0.5;
    }
    return turb;
}

// Crack pattern for burning paper edges
// Paper develops cracks and splits as it burns
float crack_pattern(vec2 uv, float seed) {
    float crack = 0.0;
    // Voronoi-like cracks
    for (float i = 0.0; i < 8.0; i += 1.0) {
        vec2 cell = vec2(
            hash1(i * 13.7 + seed),
            hash1(i * 17.3 + 100.0 + seed)
        );
        float d = length(fract(uv) - cell);
        crack = max(crack, 1.0 - smoothstep(0.0, 0.08, d));
    }
    return crack;
}

// Calculate wave offset
float calc_wave_offset(float pos, float t, float wave_fade) {
    float wave_offset = 0.0;
    wave_offset += sin(pos * 8.0 + t * 0.2) * 0.035;
    wave_offset += sin(pos * 12.0 + t * 0.4) * 0.025;
    wave_offset += sin(pos * 18.0 - t * 0.5) * 0.018;
    wave_offset += sin(pos * 25.0 - t * 0.7) * 0.015;
    wave_offset += sin(pos * 32.0 + t * 0.9) * 0.012;
    wave_offset += sin(pos * 45.0 + t * 1.2) * 0.008;
    wave_offset += sin(pos * 60.0 - t * 1.5) * 0.005;
    wave_offset += sin(pos * 80.0 + t * 1.8) * 0.003;
    wave_offset += (smooth_hash(pos * 30.0, t * 0.5) - 0.5) * 0.015;
    wave_offset += (smooth_hash(pos * 50.0, t * 0.7) - 0.5) * 0.008;
    return wave_offset * wave_fade;
}

// Calculate burn velocity (time derivative of wave offset)
float calc_burn_velocity(float pos, float t, float wave_fade) {
    float velocity = 0.0;
    velocity += 0.2 * cos(pos * 8.0 + t * 0.2) * 0.035;
    velocity += 0.4 * cos(pos * 12.0 + t * 0.4) * 0.025;
    velocity += -0.5 * cos(pos * 18.0 - t * 0.5) * 0.018;
    velocity += -0.7 * cos(pos * 25.0 - t * 0.7) * 0.015;
    velocity += 0.9 * cos(pos * 32.0 + t * 0.9) * 0.012;
    velocity += 1.2 * cos(pos * 45.0 + t * 1.2) * 0.008;
    velocity += -1.5 * cos(pos * 60.0 - t * 1.5) * 0.005;
    velocity += 1.8 * cos(pos * 80.0 + t * 1.8) * 0.003;
    
    float noise_vel1 = (smooth_hash(pos * 30.0, t * 0.5 + 0.1) - smooth_hash(pos * 30.0, t * 0.5)) * 0.5 * 0.015;
    float noise_vel2 = (smooth_hash(pos * 50.0, t * 0.7 + 0.1) - smooth_hash(pos * 50.0, t * 0.7)) * 0.7 * 0.008;
    velocity += noise_vel1 + noise_vel2;
    
    return abs(velocity) * wave_fade * 10.0;
}

// Velocity heat color with blue core support
vec3 velocity_heat_color(vec3 base_color, float velocity, float proximity) {
    float heat = smoothstep(0.0, 0.4, velocity);
    
    vec3 orange_hot = vec3(1.0, 0.5, 0.0);
    vec3 red_hot = vec3(1.0, 0.2, 0.05);
    vec3 white_hot = vec3(0.5, 0.0, 0.0);
    
    vec3 heat_color = base_color;
    heat_color = mix(heat_color, orange_hot, smoothstep(0.0, 0.25, heat));
    heat_color = mix(heat_color, red_hot, smoothstep(0.2, 0.5, heat));
    heat_color = mix(heat_color, white_hot, smoothstep(0.5, 0.9, heat));
    
    float boosted_proximity = smoothstep(0.0, 0.7, proximity);
    return mix(base_color, heat_color, boosted_proximity);
}

// Calculate rising ash particles for a given edge
// From lecture: burning gases are lighter than air and tend to rise
float calc_rising_ash(vec2 uv, float edge_start, float edge_end, float burn_edge, float t, float inv_aspect, bool is_vertical) {
    float ash_particles = 0.0;
    for (float i = 0.0; i < 6.0; i += 1.0) {
        float spawn_pos = hash1(i + floor(t * 0.2) * 7.0 + burn_edge * 100.0) * (edge_end - edge_start) + edge_start;
        float life = fract(t * 0.12 + i * 0.167);
        float rise_height = life * 0.18;
        float drift = sin(life * 6.28318 + i * 2.0) * 0.025 * life;
        
        float px, py;
        if (is_vertical) {
            px = burn_edge + rise_height + drift;
            py = spawn_pos;
        } else {
            px = spawn_pos + drift;
            py = burn_edge + rise_height;
        }
        
        float ash_size = 0.012 * (1.0 - life * 0.6);
        float ash_dist = length(vec2((uv.x - px) * inv_aspect, uv.y - py));
        float ash_alpha = smoothstep(ash_size, 0.0, ash_dist) * (1.0 - life) * (0.6 + 0.4 * sin(t * 5.0 + i * 3.0));
        
        ash_particles += ash_alpha;
    }
    return ash_particles * 0.5;
}

// Calculate smoke wisps above burn edge
// From lecture: fire creates smoke that rises and spreads
float calc_smoke_wisps(vec2 uv, float dist_from_burn, float t, float progress_fade) {
    float smoke_dist = dist_from_burn - 0.015;
    if (smoke_dist > 0.0 && smoke_dist < 0.25) {
        float smoke_density = smoothstep(0.25, 0.0, smoke_dist);
        
        // Wispy noise pattern - multiple octaves for detail
        float smoke_noise = smooth_noise2D(vec2(uv.x * 25.0, uv.y * 18.0 - t * 0.4));
        smoke_noise *= smooth_noise2D(vec2(uv.x * 45.0 + t * 0.25, uv.y * 30.0 - t * 0.3));
        smoke_noise += smooth_noise2D(vec2(uv.x * 80.0 - t * 0.15, uv.y * 60.0 - t * 0.5)) * 0.5;
        
        float smoke_alpha = smoke_density * smoke_noise * 0.35;
        smoke_alpha *= progress_fade;
        
        return clamp(smoke_alpha, 0.0, 0.6);
    }
    return 0.0;
}

void main()
{
    int burn_side = 4;
    float width = size.x;
    float height = size.y;
    
    float burn_progress = progress;
    if (direction == 1) burn_progress = 1.0 - burn_progress;
    
    float t = burn_progress * flame_speed * 10.0;
    
    float wave_fade = smoothstep(0.0, 0.1, progress) * smoothstep(1.0, 0.85, progress);
    float side_progress = (burn_side == 4) ? progress * 0.5 : progress;
    
    // Paper fiber at this pixel (affects burn edge shape)
    float fiber = paper_fiber(uvpos, t);
    
    // Fiber-based offset: thin areas (low fiber) burn faster/further
    float fiber_offset = (fiber - 0.5) * 0.025;
    
    // Bottom edge
    float wave_bottom = calc_wave_offset(uvpos.x, t, wave_fade);
    float effective_bottom = (burn_side == 0 || burn_side == 4) ? side_progress + wave_bottom + fiber_offset : 0.0;
    float dist_from_bottom = uvpos.y - effective_bottom;
    
    // Top edge
    float wave_top = calc_wave_offset(uvpos.x, t + 5.0, wave_fade);
    float effective_top = (burn_side == 1 || burn_side == 4) ? 1.0 - side_progress - wave_top - fiber_offset : 1.0;
    float dist_from_top = effective_top - uvpos.y;
    
    // Left edge
    float wave_left = calc_wave_offset(uvpos.y, t + 10.0, wave_fade);
    float effective_left = (burn_side == 2 || burn_side == 4) ? side_progress + wave_left + fiber_offset : 0.0;
    float dist_from_left = uvpos.x - effective_left;
    
    // Right edge
    float wave_right = calc_wave_offset(uvpos.y, t + 15.0, wave_fade);
    float effective_right = (burn_side == 3 || burn_side == 4) ? 1.0 - side_progress - wave_right - fiber_offset : 1.0;
    float dist_from_right = effective_right - uvpos.x;
    
    bool inside_unburned = (uvpos.y >= effective_bottom && uvpos.y <= effective_top &&
                            uvpos.x >= effective_left && uvpos.x <= effective_right);
    
    float dist_from_burn = 1000.0;
    if (burn_side == 0 || burn_side == 4) dist_from_burn = min(dist_from_burn, dist_from_bottom);
    if (burn_side == 1 || burn_side == 4) dist_from_burn = min(dist_from_burn, dist_from_top);
    if (burn_side == 2 || burn_side == 4) dist_from_burn = min(dist_from_burn, dist_from_left);
    if (burn_side == 3 || burn_side == 4) dist_from_burn = min(dist_from_burn, dist_from_right);

    // Early exit for far pixels
    if (dist_from_burn > 0.5) {
        gl_FragColor = get_pixel(uvpos);
        return;
    }
    if (dist_from_burn < -0.08) {
        gl_FragColor = vec4(0.0);
        return;
    }

    float end_blur = smoothstep(0.75, 0.95, progress);
    float distort_fade = smoothstep(0.0, 0.1, progress) * smoothstep(1.0, 0.85, progress);
    float inv_aspect = width / height;

    // === PAPER CURLING EFFECT ===
    // Real paper curls at burning edges due to heat shrinkage
    vec2 curl_offset = vec2(0.0);
    float curl_intensity = smoothstep(0.12, 0.0, dist_from_burn) * smoothstep(-0.01, 0.02, dist_from_burn);
    curl_intensity *= 0.95 * distort_fade;
    
    if (burn_side == 0 || burn_side == 4) {
        curl_offset.y += curl_intensity * smoothstep(effective_bottom + 0.08, effective_bottom, uvpos.y);
    }
    if (burn_side == 1 || burn_side == 4) {
        curl_offset.y -= curl_intensity * smoothstep(effective_top - 0.08, effective_top, uvpos.y);
    }
    if (burn_side == 2 || burn_side == 4) {
        curl_offset.x += curl_intensity * smoothstep(effective_left + 0.08, effective_left, uvpos.x);
    }
    if (burn_side == 3 || burn_side == 4) {
        curl_offset.x -= curl_intensity * smoothstep(effective_right - 0.08, effective_right, uvpos.x);
    }

    // === HEAT DISTORTION ===
    vec2 total_distort = curl_offset;
    
    if (burn_side == 0 || burn_side == 4) {
        float edge_proximity = smoothstep(0.15, 0.0, dist_from_bottom) * smoothstep(-0.02, 0.02, dist_from_bottom);
        float wave_deriv = cos(uvpos.x * 8.0 + t * 0.2) * 0.035 * 8.0
                         + cos(uvpos.x * 12.0 + t * 0.4) * 0.025 * 12.0
                         + cos(uvpos.x * 18.0 - t * 0.5) * 0.018 * 18.0 * -1.0
                         + cos(uvpos.x * 25.0 - t * 0.7) * 0.015 * 25.0 * -1.0
                         + cos(uvpos.x * 32.0 + t * 0.9) * 0.012 * 32.0
                         + cos(uvpos.x * 45.0 + t * 1.2) * 0.008 * 45.0
                         + cos(uvpos.x * 60.0 - t * 1.5) * 0.005 * 60.0 * -1.0
                         + cos(uvpos.x * 80.0 + t * 1.8) * 0.003 * 80.0
                         + smooth_hash_deriv(uvpos.x * 30.0, t * 0.5, 30.0) * 0.015
                         + smooth_hash_deriv(uvpos.x * 50.0, t * 0.7, 50.0) * 0.008;
        wave_deriv *= wave_fade;
        float flicker = 0.8 + 0.2 * sin(t * 8.0 + uvpos.x * 30.0);
        total_distort.y += edge_proximity * 0.015 * flicker;
        total_distort.x += edge_proximity * wave_deriv * 0.3 * flicker;
    }
    
    if (burn_side == 1 || burn_side == 4) {
        float edge_proximity = smoothstep(0.15, 0.0, dist_from_top) * smoothstep(-0.02, 0.02, dist_from_top);
        float wave_deriv = cos(uvpos.x * 8.0 + t * 0.2) * 0.035 * 8.0
                         + cos(uvpos.x * 12.0 + t * 0.4) * 0.025 * 12.0
                         + cos(uvpos.x * 18.0 - t * 0.5) * 0.018 * 18.0 * -1.0
                         + cos(uvpos.x * 25.0 - t * 0.7) * 0.015 * 25.0 * -1.0
                         + cos(uvpos.x * 32.0 + t * 0.9) * 0.012 * 32.0
                         + cos(uvpos.x * 45.0 + t * 1.2) * 0.008 * 45.0
                         + cos(uvpos.x * 60.0 - t * 1.5) * 0.005 * 60.0 * -1.0
                         + cos(uvpos.x * 80.0 + t * 1.8) * 0.003 * 80.0
                         + smooth_hash_deriv(uvpos.x * 30.0, t * 0.5, 30.0) * 0.015
                         + smooth_hash_deriv(uvpos.x * 50.0, t * 0.7, 50.0) * 0.008;
        wave_deriv *= wave_fade;
        float flicker = 0.8 + 0.2 * sin(t * 7.0 + uvpos.x * 25.0 + 2.0);
        total_distort.y -= edge_proximity * 0.015 * flicker;
        total_distort.x += edge_proximity * wave_deriv * 0.3 * flicker;
    }
    
    if (burn_side == 2 || burn_side == 4) {
        float edge_proximity = smoothstep(0.15, 0.0, dist_from_left) * smoothstep(-0.02, 0.02, dist_from_left);
        float wave_deriv = cos(uvpos.y * 12.0 + t * 0.4 + 10.0) * 0.025 * 12.0
                         + cos(uvpos.y * 25.0 - t * 0.7 + 10.0) * 0.015 * 25.0 * -1.0
                         + cos(uvpos.y * 45.0 + t * 1.2 + 10.0) * 0.008 * 45.0;
        wave_deriv *= wave_fade;
        float flicker = 0.8 + 0.2 * sin(t * 9.0 + uvpos.y * 28.0 + 4.0);
        total_distort.x += edge_proximity * 0.015 * flicker;
        total_distort.y += edge_proximity * wave_deriv * 0.3 * flicker;
    }
    
    if (burn_side == 3 || burn_side == 4) {
        float edge_proximity = smoothstep(0.15, 0.0, dist_from_right) * smoothstep(-0.02, 0.02, dist_from_right);
        float wave_deriv = cos(uvpos.y * 12.0 + t * 0.4 + 15.0) * 0.025 * 12.0
                         + cos(uvpos.y * 25.0 - t * 0.7 + 15.0) * 0.015 * 25.0 * -1.0
                         + cos(uvpos.y * 45.0 + t * 1.2 + 15.0) * 0.008 * 45.0;
        wave_deriv *= wave_fade;
        float flicker = 0.8 + 0.2 * sin(t * 6.0 + uvpos.y * 32.0 + 6.0);
        total_distort.x -= edge_proximity * 0.015 * flicker;
        total_distort.y += edge_proximity * wave_deriv * 0.3 * flicker;
    }
    
    total_distort *= distort_fade;
    
    // === IMPROVED HEAT SHIMMER - Rising effect ===
    // From lecture: burning gases rise due to being lighter than air
    float heat_zone = smoothstep(0.12, 0.0, dist_from_burn) * smoothstep(-0.02, 0.02, dist_from_burn);
    heat_zone *= distort_fade;
    
    // Standard shimmer
    total_distort += vec2(
        sin(uvpos.y * 50.0 + t * 8.0) * heat_zone * 0.008,
        cos(uvpos.x * 45.0 + t * 7.0) * heat_zone * 0.008
    );
    
    // Rising heat shimmer - heat rises more than it moves sideways
    float rising_shimmer = sin(uvpos.x * 60.0 + t * 4.0 - uvpos.y * 30.0) * heat_zone * 0.006;
    rising_shimmer += sin(uvpos.x * 90.0 + t * 6.0 - uvpos.y * 45.0) * heat_zone * 0.004;
    total_distort.y += rising_shimmer;
    
    vec2 distort_uv = clamp(uvpos + total_distort, 0.0, 1.0);
    vec4 bg = get_pixel(distort_uv);
    
    if (!inside_unburned) {
        bg = vec4(0.0);
    }
    
    // === SHADOW/SMOKE AND SECONDARY ILLUMINATION ===
    if (inside_unburned) {
        float shadow_zone = smoothstep(0.15, 0.0, dist_from_burn);
        float shadow_intensity = clamp(shadow_zone * 0.6, 0.0, 0.85);
        float shadow_delay = 0.05;
        float shadow_fade_duration = 0.15;
        float shadow_progress_factor;
        if (direction == 1) {
            shadow_progress_factor = smoothstep(shadow_delay, shadow_delay + shadow_fade_duration, progress);
        } else {
            shadow_progress_factor = smoothstep(shadow_delay, shadow_delay + shadow_fade_duration, progress);
        }
        shadow_progress_factor *= smoothstep(1.0, 0.85, progress);
        vec3 shadow_color = vec3(0.05, 0.03, 0.02);
        bg.rgb = mix(bg.rgb, shadow_color, shadow_intensity * 1.7 * shadow_progress_factor);
        
        // Heat discoloration zone - paper yellows/browns before burning
        // From lecture: temperature affects visual appearance
        float heat_discolor = smoothstep(0.08, 0.01, dist_from_burn);
        vec3 scorched_tint = vec3(0.95, 0.85, 0.7);
        bg.rgb *= mix(vec3(1.0), scorched_tint, heat_discolor * 0.4);
        
        // === SECONDARY ILLUMINATION (Radiosity-lite) ===
        // From lecture: fire emits light and illuminates surrounding objects
        float illumination_zone = smoothstep(0.30, 0.0, dist_from_burn);
        float flicker_illum = 0.8 + 0.2 * sin(t * 12.0 + uvpos.x * 20.0) * sin(t * 9.0 + uvpos.y * 15.0);
        flicker_illum *= 0.9 + 0.1 * sin(t * 17.0 + uvpos.x * 35.0);
        vec3 fire_light = vec3(1.0, 0.55, 0.15) * illumination_zone * 0.18 * flicker_illum;
        fire_light *= distort_fade;
        // Don't illuminate heavily shadowed areas as much
        bg.rgb += fire_light * (1.0 - shadow_intensity * 0.5);
    }

    vec4 result = bg;
    
    float burn_size_base = 4.0 * (1.0 + end_blur * 2.0);
    vec3 burn_line_total = vec3(0.0);
    float burn_alpha_total = 0.0;
    float progress_fade = clamp(progress * 10.0, 0.0, 1.0);

    // Turbulence for flame flickering
    float flame_turb = turbulence(vec2(uvpos.x * 15.0, uvpos.y * 15.0 + t * 0.5), t);

    // === BOTTOM EDGE ===
    if ((burn_side == 0 || burn_side == 4) && uvpos.x >= effective_left && uvpos.x <= effective_right) {
        float edge_dist = dist_from_bottom;
        float edge_pos = uvpos.x;
        float burn_velocity = calc_burn_velocity(edge_pos, t, wave_fade);
        
        // Fiber affects thickness - denser areas have thicker burn line
        float thickness_var = 0.6 + 0.4 * sin(edge_pos * 15.0 + t * 1.5)
                            + 0.3 * sin(edge_pos * 35.0 - t * 2.3)
                            + 0.2 * sin(edge_pos * 70.0 + t * 3.7)
                            + 0.15 * (smooth_hash(edge_pos * 20.0, t * 0.8) - 0.5)
                            + 0.12 * (fiber - 0.5);
        thickness_var = clamp(thickness_var, 0.4, 1.4);
        float burn_size = burn_size_base * thickness_var;
        
        // Fiber adds micro-variation to edge shape
        float fiber_edge = (fiber - 0.5) * 0.008;
        float wave = sin(edge_pos * 40.0 + t * 2.0) * 0.003 + sin(edge_pos * 80.0 - t * 3.0) * 0.002;
        float edge_noise = smooth_hash(edge_pos * width * 0.5, t * 2.0) * 0.01;
        float adjusted_dist = edge_dist + wave + edge_noise + fiber_edge;
        
        float abs_adj = abs(adjusted_dist);
        
        // Apply turbulence to glow intensities for realistic flickering
        float turb_mod = 0.7 + 0.3 * flame_turb;
        
        float ember_core = smoothstep(0.012 * burn_size, 0.0, abs_adj) * smoothstep(-0.005 * burn_size, 0.005 * burn_size, adjusted_dist);
        float inner_glow = smoothstep(0.025 * burn_size, 0.0, abs_adj) * smoothstep(-0.01 * burn_size, 0.01 * burn_size, adjusted_dist);
        float outer_glow = smoothstep(0.05 * burn_size, 0.0, abs_adj) * smoothstep(-0.02 * burn_size, 0.02 * burn_size, adjusted_dist);
        
        ember_core *= turb_mod;
        inner_glow *= 0.85 + 0.15 * flame_turb;
        outer_glow *= 0.9 + 0.1 * flame_turb;
        
        float edge_proximity = smoothstep(0.08, 0.0, abs_adj);
        
        // Orange ember particles
        float ember_particle = 0.0;
        for (float i = 0.0; i < 3.0; i += 1.0) {
            float px = smooth_hash1(i + t * 0.5) * (effective_right - effective_left) + effective_left;
            float py = effective_bottom + sin(t * (2.0 + i) + i * 3.14159) * 0.015 * burn_size;
            float spark_dist = length(vec2((uvpos.x - px) * inv_aspect, uvpos.y - py));
            ember_particle += smoothstep(0.02 * burn_size, 0.0, spark_dist) * (0.5 + 0.5 * sin(t * 10.0 + i * 5.0));
        }
        
        // Red ember particles
        float red_ember_particle = 0.0;
        for (float i = 0.0; i < 4.0; i += 1.0) {
            float px = smooth_hash1(i + t * 0.4 + 100.0) * (effective_right - effective_left) + effective_left;
            float py = effective_bottom + sin(t * (1.5 + i * 0.7) + i * 2.718) * 0.018 * burn_size;
            float spark_dist = length(vec2((uvpos.x - px) * inv_aspect, uvpos.y - py));
            red_ember_particle += smoothstep(0.025 * burn_size, 0.0, spark_dist) * (0.4 + 0.6 * sin(t * 8.0 + i * 4.0));
        }
        
        // Rising ash particles
        float ash_particles = calc_rising_ash(uvpos, effective_left, effective_right, effective_bottom, t, inv_aspect, false);
        
        // Base colors
        vec3 base_outer = vec3(0.8, 0.2, 0.0);
        vec3 base_inner = vec3(1.0, 0.6, 0.1);
        vec3 base_core = vec3(1.0, 0.95, 0.7);
        vec3 base_ember = vec3(1.0, 0.5, 0.0);
        vec3 base_red_ember = vec3(1.0, 0.1, 0.0);
        vec3 ash_color = vec3(0.12, 0.10, 0.08);
        
        // Blue flame at hottest core (oxygen-rich combustion)
        vec3 blue_flame = vec3(0.3, 0.5, 1.0);
        float blue_core = smoothstep(0.006 * burn_size, 0.0, abs_adj) * 
                          smoothstep(-0.002 * burn_size, 0.002 * burn_size, adjusted_dist);
        
        vec3 hot_outer = velocity_heat_color(base_outer, burn_velocity, edge_proximity);
        vec3 hot_inner = velocity_heat_color(base_inner, burn_velocity, edge_proximity);
        vec3 hot_core = velocity_heat_color(base_core, burn_velocity * 1.5, edge_proximity);
        
        vec3 burn_line = hot_outer * outer_glow 
                       + hot_inner * inner_glow 
                       + hot_core * ember_core 
                       + blue_flame * blue_core * 0.35
                       + base_ember * ember_particle
                       + base_red_ember * red_ember_particle
                       + ash_color * ash_particles;
        
        // Charred edge with crack pattern
        float char_zone = smoothstep(0.005, -0.03, edge_dist);
        float char_texture = fiber * 0.4 + 0.6;
        float cracks = crack_pattern(uvpos * 12.0 + vec2(t * 0.05, 0.0), edge_pos * 10.0);
        vec3 char_color = vec3(0.06, 0.03, 0.02) * char_texture;
        char_color = mix(char_color, vec3(0.02, 0.01, 0.005), cracks * 0.6);
        burn_line = mix(burn_line, char_color, char_zone * 0.55);
        
        float burn_alpha = max(max(outer_glow, inner_glow), max(ember_core, max(ember_particle * 0.8, max(red_ember_particle * 0.9, ash_particles * 0.5))));
        burn_alpha = max(burn_alpha, blue_core * 0.4);
        burn_alpha *= (0.9 + 0.2 * sin(t * 20.0 + edge_pos * 50.0) * sin(t * 17.0));
        burn_alpha *= progress_fade;
        
        burn_line_total += burn_line * burn_alpha;
        burn_alpha_total = max(burn_alpha_total, burn_alpha);
    }

    // === TOP EDGE ===
    if ((burn_side == 1 || burn_side == 4) && uvpos.x >= effective_left && uvpos.x <= effective_right) {
        float edge_dist = dist_from_top;
        float edge_pos = uvpos.x;
        float burn_velocity = calc_burn_velocity(edge_pos, t + 5.0, wave_fade);
        
        float thickness_var = 0.6 + 0.4 * sin(edge_pos * 15.0 + t * 1.5 + 3.0)
                            + 0.3 * sin(edge_pos * 35.0 - t * 2.3 + 5.0)
                            + 0.2 * sin(edge_pos * 70.0 + t * 3.7 + 7.0)
                            + 0.15 * (smooth_hash(edge_pos * 20.0 + 50.0, t * 0.8) - 0.5)
                            + 0.12 * (fiber - 0.5);
        thickness_var = clamp(thickness_var, 0.4, 1.4);
        float burn_size = burn_size_base * thickness_var;
        
        float fiber_edge = (fiber - 0.5) * 0.008;
        float wave = sin(edge_pos * 40.0 + t * 2.0 + 5.0) * 0.003 + sin(edge_pos * 80.0 - t * 3.0 + 7.0) * 0.002;
        float edge_noise = smooth_hash(edge_pos * width * 0.5, t * 2.0 + 1.0) * 0.01;
        float adjusted_dist = edge_dist + wave + edge_noise + fiber_edge;
        
        float abs_adj = abs(adjusted_dist);
        float turb_mod = 0.7 + 0.3 * flame_turb;
        
        float ember_core = smoothstep(0.012 * burn_size, 0.0, abs_adj) * smoothstep(-0.005 * burn_size, 0.005 * burn_size, adjusted_dist) * turb_mod;
        float inner_glow = smoothstep(0.025 * burn_size, 0.0, abs_adj) * smoothstep(-0.01 * burn_size, 0.01 * burn_size, adjusted_dist) * (0.85 + 0.15 * flame_turb);
        float outer_glow = smoothstep(0.05 * burn_size, 0.0, abs_adj) * smoothstep(-0.02 * burn_size, 0.02 * burn_size, adjusted_dist) * (0.9 + 0.1 * flame_turb);
        float edge_proximity = smoothstep(0.08, 0.0, abs_adj);
        
        float ember_particle = 0.0;
        for (float i = 0.0; i < 3.0; i += 1.0) {
            float px = smooth_hash1(i + t * 0.5 + 10.0) * (effective_right - effective_left) + effective_left;
            float py = effective_top + sin(t * (2.0 + i) + i * 3.14159 + 2.0) * 0.015 * burn_size;
            float spark_dist = length(vec2((uvpos.x - px) * inv_aspect, uvpos.y - py));
            ember_particle += smoothstep(0.02 * burn_size, 0.0, spark_dist) * (0.5 + 0.5 * sin(t * 10.0 + i * 5.0 + 1.0));
        }
        
        float red_ember_particle = 0.0;
        for (float i = 0.0; i < 4.0; i += 1.0) {
            float px = smooth_hash1(i + t * 0.4 + 110.0) * (effective_right - effective_left) + effective_left;
            float py = effective_top + sin(t * (1.5 + i * 0.7) + i * 2.718 + 1.5) * 0.018 * burn_size;
            float spark_dist = length(vec2((uvpos.x - px) * inv_aspect, uvpos.y - py));
            red_ember_particle += smoothstep(0.025 * burn_size, 0.0, spark_dist) * (0.4 + 0.6 * sin(t * 8.0 + i * 4.0 + 1.0));
        }
        
        // For top edge, ash falls/drifts rather than rises
        float ash_particles = 0.0;
        for (float i = 0.0; i < 5.0; i += 1.0) {
            float spawn_x = hash1(i + floor(t * 0.25) * 11.0) * (effective_right - effective_left) + effective_left;
            float life = fract(t * 0.1 + i * 0.2);
            float fall_dist = life * 0.12;
            float drift = sin(life * 6.28318 + i * 1.5) * 0.02 * life;
            float px = spawn_x + drift;
            float py = effective_top - fall_dist;
            float ash_size = 0.01 * (1.0 - life * 0.5);
            float ash_dist = length(vec2((uvpos.x - px) * inv_aspect, uvpos.y - py));
            ash_particles += smoothstep(ash_size, 0.0, ash_dist) * (1.0 - life) * 0.4;
        }
        
        vec3 base_outer = vec3(0.8, 0.2, 0.0);
        vec3 base_inner = vec3(1.0, 0.6, 0.1);
        vec3 base_core = vec3(1.0, 0.95, 0.7);
        vec3 base_ember = vec3(1.0, 0.5, 0.0);
        vec3 base_red_ember = vec3(1.0, 0.1, 0.0);
        vec3 ash_color = vec3(0.12, 0.10, 0.08);
        vec3 blue_flame = vec3(0.3, 0.5, 1.0);
        
        float blue_core = smoothstep(0.006 * burn_size, 0.0, abs_adj) * 
                          smoothstep(-0.002 * burn_size, 0.002 * burn_size, adjusted_dist);
        
        vec3 hot_outer = velocity_heat_color(base_outer, burn_velocity, edge_proximity);
        vec3 hot_inner = velocity_heat_color(base_inner, burn_velocity, edge_proximity);
        vec3 hot_core = velocity_heat_color(base_core, burn_velocity * 1.5, edge_proximity);
        
        vec3 burn_line = hot_outer * outer_glow 
                       + hot_inner * inner_glow 
                       + hot_core * ember_core 
                       + blue_flame * blue_core * 0.35
                       + base_ember * ember_particle
                       + base_red_ember * red_ember_particle
                       + ash_color * ash_particles;
        
        float char_zone = smoothstep(0.005, -0.03, edge_dist);
        float char_texture = fiber * 0.4 + 0.6;
        float cracks = crack_pattern(uvpos * 12.0 + vec2(t * 0.05, 0.0), edge_pos * 10.0 + 50.0);
        vec3 char_color = vec3(0.06, 0.03, 0.02) * char_texture;
        char_color = mix(char_color, vec3(0.02, 0.01, 0.005), cracks * 0.6);
        burn_line = mix(burn_line, char_color, char_zone * 0.55);
        
        float burn_alpha = max(max(outer_glow, inner_glow), max(ember_core, max(ember_particle * 0.8, max(red_ember_particle * 0.9, ash_particles * 0.5))));
        burn_alpha = max(burn_alpha, blue_core * 0.4);
        burn_alpha *= (0.9 + 0.2 * sin(t * 20.0 + edge_pos * 50.0 + 3.0) * sin(t * 17.0));
        burn_alpha *= progress_fade;
        
        burn_line_total += burn_line * burn_alpha;
        burn_alpha_total = max(burn_alpha_total, burn_alpha);
    }

    // === LEFT EDGE ===
    if ((burn_side == 2 || burn_side == 4) && uvpos.y >= effective_bottom && uvpos.y <= effective_top) {
        float edge_dist = dist_from_left;
        float edge_pos = uvpos.y;
        float burn_velocity = calc_burn_velocity(edge_pos, t + 10.0, wave_fade);
        
        float thickness_var = 0.6 + 0.4 * sin(edge_pos * 15.0 + t * 1.5 + 6.0)
                            + 0.3 * sin(edge_pos * 35.0 - t * 2.3 + 10.0)
                            + 0.2 * sin(edge_pos * 70.0 + t * 3.7 + 14.0)
                            + 0.15 * (smooth_hash(edge_pos * 20.0 + 100.0, t * 0.8) - 0.5)
                            + 0.12 * (fiber - 0.5);
        thickness_var = clamp(thickness_var, 0.4, 1.4);
        float burn_size = burn_size_base * thickness_var;
        
        float fiber_edge = (fiber - 0.5) * 0.008;
        float wave = sin(edge_pos * 40.0 + t * 2.0 + 10.0) * 0.003 + sin(edge_pos * 80.0 - t * 3.0 + 14.0) * 0.002;
        float edge_noise = smooth_hash(edge_pos * height * 0.5, t * 2.0 + 2.0) * 0.01;
        float adjusted_dist = edge_dist + wave + edge_noise + fiber_edge;
        
        float abs_adj = abs(adjusted_dist);
        float turb_mod = 0.7 + 0.3 * flame_turb;
        
        float ember_core = smoothstep(0.012 * burn_size, 0.0, abs_adj) * smoothstep(-0.005 * burn_size, 0.005 * burn_size, adjusted_dist) * turb_mod;
        float inner_glow = smoothstep(0.025 * burn_size, 0.0, abs_adj) * smoothstep(-0.01 * burn_size, 0.01 * burn_size, adjusted_dist) * (0.85 + 0.15 * flame_turb);
        float outer_glow = smoothstep(0.05 * burn_size, 0.0, abs_adj) * smoothstep(-0.02 * burn_size, 0.02 * burn_size, adjusted_dist) * (0.9 + 0.1 * flame_turb);
        float edge_proximity = smoothstep(0.08, 0.0, abs_adj);
        
        float ember_particle = 0.0;
        for (float i = 0.0; i < 3.0; i += 1.0) {
            float py = smooth_hash1(i + t * 0.5 + 20.0) * (effective_top - effective_bottom) + effective_bottom;
            float px = effective_left + sin(t * (2.0 + i) + i * 3.14159 + 4.0) * 0.015 * burn_size;
            float spark_dist = length(vec2((uvpos.x - px) * inv_aspect, uvpos.y - py));
            ember_particle += smoothstep(0.02 * burn_size, 0.0, spark_dist) * (0.5 + 0.5 * sin(t * 10.0 + i * 5.0 + 2.0));
        }
        
        float red_ember_particle = 0.0;
        for (float i = 0.0; i < 4.0; i += 1.0) {
            float py = smooth_hash1(i + t * 0.4 + 120.0) * (effective_top - effective_bottom) + effective_bottom;
            float px = effective_left + sin(t * (1.5 + i * 0.7) + i * 2.718 + 3.0) * 0.018 * burn_size;
            float spark_dist = length(vec2((uvpos.x - px) * inv_aspect, uvpos.y - py));
            red_ember_particle += smoothstep(0.025 * burn_size, 0.0, spark_dist) * (0.4 + 0.6 * sin(t * 8.0 + i * 4.0 + 2.0));
        }
        
        float ash_particles = calc_rising_ash(uvpos, effective_bottom, effective_top, effective_left, t + 20.0, inv_aspect, true);
        
        vec3 base_outer = vec3(0.8, 0.2, 0.0);
        vec3 base_inner = vec3(1.0, 0.6, 0.1);
        vec3 base_core = vec3(1.0, 0.95, 0.7);
        vec3 base_ember = vec3(1.0, 0.5, 0.0);
        vec3 base_red_ember = vec3(1.0, 0.1, 0.0);
        vec3 ash_color = vec3(0.12, 0.10, 0.08);
        vec3 blue_flame = vec3(0.3, 0.5, 1.0);
        
        float blue_core = smoothstep(0.006 * burn_size, 0.0, abs_adj) * 
                          smoothstep(-0.002 * burn_size, 0.002 * burn_size, adjusted_dist);
        
        vec3 hot_outer = velocity_heat_color(base_outer, burn_velocity, edge_proximity);
        vec3 hot_inner = velocity_heat_color(base_inner, burn_velocity, edge_proximity);
        vec3 hot_core = velocity_heat_color(base_core, burn_velocity * 1.5, edge_proximity);
        
        vec3 burn_line = hot_outer * outer_glow 
                       + hot_inner * inner_glow 
                       + hot_core * ember_core 
                       + blue_flame * blue_core * 0.35
                       + base_ember * ember_particle
                       + base_red_ember * red_ember_particle
                       + ash_color * ash_particles;
        
        float char_zone = smoothstep(0.005, -0.03, edge_dist);
        float char_texture = fiber * 0.4 + 0.6;
        float cracks = crack_pattern(uvpos * 12.0 + vec2(0.0, t * 0.05), edge_pos * 10.0 + 100.0);
        vec3 char_color = vec3(0.06, 0.03, 0.02) * char_texture;
        char_color = mix(char_color, vec3(0.02, 0.01, 0.005), cracks * 0.6);
        burn_line = mix(burn_line, char_color, char_zone * 0.55);
        
        float burn_alpha = max(max(outer_glow, inner_glow), max(ember_core, max(ember_particle * 0.8, max(red_ember_particle * 0.9, ash_particles * 0.5))));
        burn_alpha = max(burn_alpha, blue_core * 0.4);
        burn_alpha *= (0.9 + 0.2 * sin(t * 20.0 + edge_pos * 50.0 + 6.0) * sin(t * 17.0));
        burn_alpha *= progress_fade;
        
        burn_line_total += burn_line * burn_alpha;
        burn_alpha_total = max(burn_alpha_total, burn_alpha);
    }

    // === RIGHT EDGE ===
    if ((burn_side == 3 || burn_side == 4) && uvpos.y >= effective_bottom && uvpos.y <= effective_top) {
        float edge_dist = dist_from_right;
        float edge_pos = uvpos.y;
        float burn_velocity = calc_burn_velocity(edge_pos, t + 15.0, wave_fade);
        
        float thickness_var = 0.6 + 0.4 * sin(edge_pos * 15.0 + t * 1.5 + 9.0)
                            + 0.3 * sin(edge_pos * 35.0 - t * 2.3 + 15.0)
                            + 0.2 * sin(edge_pos * 70.0 + t * 3.7 + 21.0)
                            + 0.15 * (smooth_hash(edge_pos * 20.0 + 150.0, t * 0.8) - 0.5)
                            + 0.12 * (fiber - 0.5);
        thickness_var = clamp(thickness_var, 0.4, 1.4);
        float burn_size = burn_size_base * thickness_var;
        
        float fiber_edge = (fiber - 0.5) * 0.008;
        float wave = sin(edge_pos * 40.0 + t * 2.0 + 15.0) * 0.003 + sin(edge_pos * 80.0 - t * 3.0 + 21.0) * 0.002;
        float edge_noise = smooth_hash(edge_pos * height * 0.5, t * 2.0 + 3.0) * 0.01;
        float adjusted_dist = edge_dist + wave + edge_noise + fiber_edge;
        
        float abs_adj = abs(adjusted_dist);
        float turb_mod = 0.7 + 0.3 * flame_turb;
        
        float ember_core = smoothstep(0.012 * burn_size, 0.0, abs_adj) * smoothstep(-0.005 * burn_size, 0.005 * burn_size, adjusted_dist) * turb_mod;
        float inner_glow = smoothstep(0.025 * burn_size, 0.0, abs_adj) * smoothstep(-0.01 * burn_size, 0.01 * burn_size, adjusted_dist) * (0.85 + 0.15 * flame_turb);
        float outer_glow = smoothstep(0.05 * burn_size, 0.0, abs_adj) * smoothstep(-0.02 * burn_size, 0.02 * burn_size, adjusted_dist) * (0.9 + 0.1 * flame_turb);
        float edge_proximity = smoothstep(0.08, 0.0, abs_adj);
        
        float ember_particle = 0.0;
        for (float i = 0.0; i < 3.0; i += 1.0) {
            float py = smooth_hash1(i + t * 0.5 + 30.0) * (effective_top - effective_bottom) + effective_bottom;
            float px = effective_right + sin(t * (2.0 + i) + i * 3.14159 + 6.0) * 0.015 * burn_size;
            float spark_dist = length(vec2((uvpos.x - px) * inv_aspect, uvpos.y - py));
            ember_particle += smoothstep(0.02 * burn_size, 0.0, spark_dist) * (0.5 + 0.5 * sin(t * 10.0 + i * 5.0 + 3.0));
        }
        
        float red_ember_particle = 0.0;
        for (float i = 0.0; i < 4.0; i += 1.0) {
            float py = smooth_hash1(i + t * 0.4 + 130.0) * (effective_top - effective_bottom) + effective_bottom;
            float px = effective_right + sin(t * (1.5 + i * 0.7) + i * 2.718 + 4.5) * 0.018 * burn_size;
            float spark_dist = length(vec2((uvpos.x - px) * inv_aspect, uvpos.y - py));
            red_ember_particle += smoothstep(0.025 * burn_size, 0.0, spark_dist) * (0.4 + 0.6 * sin(t * 8.0 + i * 4.0 + 3.0));
        }
        
        float ash_particles = calc_rising_ash(uvpos, effective_bottom, effective_top, effective_right, t + 30.0, inv_aspect, true);
        
        vec3 base_outer = vec3(0.8, 0.2, 0.0);
        vec3 base_inner = vec3(1.0, 0.6, 0.1);
        vec3 base_core = vec3(1.0, 0.95, 0.7);
        vec3 base_ember = vec3(1.0, 0.5, 0.0);
        vec3 base_red_ember = vec3(1.0, 0.1, 0.0);
        vec3 ash_color = vec3(0.12, 0.10, 0.08);
        vec3 blue_flame = vec3(0.3, 0.5, 1.0);
        
        float blue_core = smoothstep(0.006 * burn_size, 0.0, abs_adj) * 
                          smoothstep(-0.002 * burn_size, 0.002 * burn_size, adjusted_dist);
        
        vec3 hot_outer = velocity_heat_color(base_outer, burn_velocity, edge_proximity);
        vec3 hot_inner = velocity_heat_color(base_inner, burn_velocity, edge_proximity);
        vec3 hot_core = velocity_heat_color(base_core, burn_velocity * 1.5, edge_proximity);
        
        vec3 burn_line = hot_outer * outer_glow 
                       + hot_inner * inner_glow 
                       + hot_core * ember_core 
                       + blue_flame * blue_core * 0.35
                       + base_ember * ember_particle
                       + base_red_ember * red_ember_particle
                       + ash_color * ash_particles;
        
        float char_zone = smoothstep(0.005, -0.03, edge_dist);
        float char_texture = fiber * 0.4 + 0.6;
        float cracks = crack_pattern(uvpos * 12.0 + vec2(0.0, t * 0.05), edge_pos * 10.0 + 150.0);
        vec3 char_color = vec3(0.06, 0.03, 0.02) * char_texture;
        char_color = mix(char_color, vec3(0.02, 0.01, 0.005), cracks * 0.6);
        burn_line = mix(burn_line, char_color, char_zone * 0.55);
        
        float burn_alpha = max(max(outer_glow, inner_glow), max(ember_core, max(ember_particle * 0.8, max(red_ember_particle * 0.9, ash_particles * 0.5))));
        burn_alpha = max(burn_alpha, blue_core * 0.4);
        burn_alpha *= (0.9 + 0.2 * sin(t * 20.0 + edge_pos * 50.0 + 9.0) * sin(t * 17.0));
        burn_alpha *= progress_fade;
        
        burn_line_total += burn_line * burn_alpha;
        burn_alpha_total = max(burn_alpha_total, burn_alpha);
    }
    
    result.rgb += burn_line_total * (1.0 - end_blur * 0.7);
    
    // === SMOKE WISPS ===
    // From lecture: fire creates smoke that rises
    float smoke_alpha = calc_smoke_wisps(uvpos, dist_from_burn, t, distort_fade);
    vec3 smoke_color = vec3(0.18, 0.15, 0.12);
    // Smoke is more visible against the burned (transparent) area
    if (!inside_unburned && dist_from_burn > -0.06) {
        result.rgb = mix(result.rgb, smoke_color, smoke_alpha * 0.7);
        result.a = max(result.a, smoke_alpha * 0.5);
    }
    
    if (end_blur > 0.0) {
        float luma = dot(result.rgb, vec3(0.299, 0.587, 0.114));
        result.rgb = mix(result.rgb, vec3(luma) * vec3(1.0, 0.7, 0.4), end_blur * 0.4);
    }
    
    gl_FragColor = clamp(result, 0.0, 1.0);
}
)";

namespace wf
{
namespace burn
{
using namespace wf::scene;
using namespace wf::animate;
using namespace wf::animation;

static std::string burn_transformer_name = "animation-burn";

wf::option_wrapper_t<double> burn_flame_speed{"extra-animations/burn_flame_speed"};
wf::option_wrapper_t<double> burn_flame_width{"extra-animations/burn_flame_width"};
wf::option_wrapper_t<double> burn_flame_height{"extra-animations/burn_flame_height"};
wf::option_wrapper_t<wf::color_t> burn_flame_color{"extra-animations/burn_flame_color"};
wf::option_wrapper_t<std::string> burn_flame_smoothness{"extra-animations/burn_flame_smoothness"};

class burn_transformer : public wf::scene::view_2d_transformer_t
{
  public:
    wayfire_view view;
    wf::output_t *output;
    OpenGL::program_t program;
    wf::auxilliary_buffer_t buffer;
    duration_t progression;

    class simple_node_render_instance_t : public wf::scene::transformer_render_instance_t<transformer_base_node_t>
    {
        wf::signal::connection_t<node_damage_signal> on_node_damaged =
            [=] (node_damage_signal *ev)
        {
            push_to_parent(ev->region);
        };

        burn_transformer *self;
        wayfire_view view;
        damage_callback push_to_parent;

      public:
        simple_node_render_instance_t(burn_transformer *self, damage_callback push_damage,
            wayfire_view view) : wf::scene::transformer_render_instance_t<transformer_base_node_t>(self,
                push_damage,
                view->get_output())
        {
            this->self = self;
            this->view = view;
            this->push_to_parent = push_damage;
            self->connect(&on_node_damaged);
        }

        ~simple_node_render_instance_t()
        {}

        void schedule_instructions(
            std::vector<render_instruction_t>& instructions,
            const wf::render_target_t& target, wf::region_t& damage) override
        {
            instructions.push_back(render_instruction_t{
                        .instance = this,
                        .target   = target,
                        .damage   = damage & self->get_bounding_box(),
                    });
        }

        void transform_damage_region(wf::region_t& damage) override
        {
            damage |= self->get_bounding_box();
        }

        void render(const wf::scene::render_instruction_t& data) override
        {
            auto bb  = self->get_children_bounding_box();
            auto pbb = self->get_padded_bounding_box();
            auto tex = wf::gles_texture_t{get_texture(1.0)};

            const float vertices[] = {
                -1.0, -1.0,
                1.0f, -1.0,
                1.0f, 1.0f,
                -1.0, 1.0f
            };
            wf::pointf_t offset1{-float(bb.x - pbb.x) / bb.width,
                -float(pbb.height - ((bb.y - pbb.y) + bb.height)) / bb.height};
            wf::pointf_t offset2{float(pbb.width) / bb.width + offset1.x,
                float(pbb.height) / bb.height + offset1.y};
            const float uv[] = {
                float(offset1.x), float(offset2.y),
                float(offset2.x), float(offset2.y),
                float(offset2.x), float(offset1.y),
                float(offset1.x), float(offset1.y),
            };
            auto progress = self->progression.progress();

            data.pass->custom_gles_subpass([&]
            {
                self->buffer.allocate({pbb.width, pbb.height});
                wf::gles::bind_render_buffer(self->buffer.get_renderbuffer());
                wf::gles_texture_t final_tex{self->buffer.get_texture()};
                OpenGL::clear(wf::color_t{0.0, 0.0, 0.0, 0.0}, GL_COLOR_BUFFER_BIT);
                self->program.use(wf::TEXTURE_TYPE_RGBA);
                self->program.attrib_pointer("position", 2, 0, vertices);
                self->program.attrib_pointer("uv_in", 2, 0, uv);
                self->program.uniform2f("size", bb.width * 1.0, bb.height * 1.0);
                self->program.uniform1f("progress", 1.0 - progress);
                self->program.uniform1i("direction", self->progression.get_direction());
                self->program.uniform1f("flame_speed", burn_flame_speed);
                self->program.uniform1f("flame_width", burn_flame_width);
                self->program.uniform1f("flame_height", burn_flame_height);
                if (std::string(burn_flame_smoothness) == "softest")
                {
                    self->program.uniform1i("flame_smooth_1", 0);
                    self->program.uniform1i("flame_smooth_2", 0);
                    self->program.uniform1i("flame_smooth_3", 0);
                    self->program.uniform1i("flame_smooth_4", 0);
                } else if (std::string(burn_flame_smoothness) == "soft")
                {
                    self->program.uniform1i("flame_smooth_1", 0);
                    self->program.uniform1i("flame_smooth_2", 1);
                    self->program.uniform1i("flame_smooth_3", 1);
                    self->program.uniform1i("flame_smooth_4", 1);
                } else if (std::string(burn_flame_smoothness) == "hard")
                {
                    self->program.uniform1i("flame_smooth_1", 1);
                    self->program.uniform1i("flame_smooth_2", 1);
                    self->program.uniform1i("flame_smooth_3", 1);
                    self->program.uniform1i("flame_smooth_4", 1);
                } else // "normal"
                {
                    self->program.uniform1i("flame_smooth_1", 1);
                    self->program.uniform1i("flame_smooth_2", 0);
                    self->program.uniform1i("flame_smooth_3", 1);
                    self->program.uniform1i("flame_smooth_4", 0);
                }

                glm::vec4 flame_color{
                    wf::color_t(burn_flame_color).r,
                    wf::color_t(burn_flame_color).g,
                    wf::color_t(burn_flame_color).b,
                    wf::color_t(burn_flame_color).a};
                self->program.uniform4f("flame_color", flame_color);

                self->program.set_active_texture(tex);
                GL_CALL(glDrawArrays(GL_TRIANGLE_FAN, 0, 4));

                wf::gles::bind_render_buffer(data.target);
                for (auto box : data.damage)
                {
                    wf::gles::render_target_logic_scissor(data.target, wlr_box_from_pixman_box(box));
                    OpenGL::render_transformed_texture(final_tex, pbb,
                        wf::gles::render_target_orthographic_projection(data.target),
                        glm::vec4(1.0, 1.0, 1.0, std::clamp((progress - 0.07) * 10.0, 0.0, 1.0)), 0);
                }

                GL_CALL(glBindTexture(GL_TEXTURE_2D, 0));
                self->program.deactivate();
                self->buffer.free();
            });
        }
    };

    burn_transformer(wayfire_view view, wf::geometry_t bbox,
        wf::animation_description_t duration) : wf::scene::view_2d_transformer_t(view)
    {
        this->view = view;
        this->progression = duration_t{wf::create_option(duration)};
        if (view->get_output())
        {
            output = view->get_output();
            output->render->add_effect(&pre_hook, wf::OUTPUT_EFFECT_PRE);
        }

        wf::gles::run_in_context([&]
        {
            program.compile(burn_vert_source, burn_frag_source);
        });
    }

    wf::geometry_t get_padded_bounding_box()
    {
        auto box     = this->get_children_bounding_box();
        auto padding = 100;
        box.x     -= padding;
        box.y     -= padding;
        box.width += padding * 2;
        box.height += padding * 2;
        return box;
    }

    wf::geometry_t get_bounding_box() override
    {
        return get_padded_bounding_box();
    }

    wf::effect_hook_t pre_hook = [=] ()
    {
        output->render->damage(this->get_bounding_box());
    };

    void gen_render_instances(std::vector<render_instance_uptr>& instances,
        damage_callback push_damage, wf::output_t *shown_on) override
    {
        instances.push_back(std::make_unique<simple_node_render_instance_t>(
            this, push_damage, view));
    }

    void init_animation(bool burn)
    {
        if (burn)
        {
            this->progression.reverse();
        }

        this->progression.start();
    }

    virtual ~burn_transformer()
    {
        if (output)
        {
            output->render->rem_effect(&pre_hook);
        }

        wf::gles::run_in_context_if_gles([&]
        {
            program.free_resources();
        });
    }
};

class burn_animation : public animation_base_t
{
    wayfire_view view;

  public:
    void init(wayfire_view view, wf::animation_description_t dur, animation_type type) override
    {
        this->view = view;
        pop_transformer(view);
        auto bbox = view->get_transformed_node()->get_bounding_box();
        auto tmgr = view->get_transformed_node();
        auto node = std::make_shared<burn_transformer>(view, bbox, dur);
        tmgr->add_transformer(node, wf::TRANSFORMER_HIGHLEVEL + 1, burn_transformer_name);
        node->init_animation(type & WF_ANIMATE_HIDING_ANIMATION);
    }

    void pop_transformer(wayfire_view view)
    {
        if (view->get_transformed_node()->get_transformer(burn_transformer_name))
        {
            view->get_transformed_node()->rem_transformer(burn_transformer_name);
        }
    }

    bool step() override
    {
        if (!view)
        {
            return false;
        }

        auto tmgr = view->get_transformed_node();
        if (!tmgr)
        {
            return false;
        }

        if (auto tr =
                tmgr->get_transformer<wf::burn::burn_transformer>(burn_transformer_name))
        {
            auto running = tr->progression.running();
            if (!running)
            {
                pop_transformer(view);
                return false;
            }

            return running;
        }

        return false;
    }

    void reverse() override
    {
        if (auto tr =
                view->get_transformed_node()->get_transformer<wf::burn::burn_transformer>(
                    burn_transformer_name))
        {
            tr->progression.reverse();
        }
    }
};
}
}
