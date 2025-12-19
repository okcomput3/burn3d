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


// Add this smooth noise function (C1 continuous - has continuous first derivative)
float smooth_hash(float p, float t) {
    float i = floor(p);
    float f = fract(p);
    // Smoothstep gives C1 continuity (smooth first derivative)
    f = f * f * (3.0 - 2.0 * f);
    return mix(hash2(vec2(i, floor(t))), hash2(vec2(i + 1.0, floor(t))), f);
}

// Derivative of smooth_hash with respect to position (for tangent calculation)
float smooth_hash_deriv(float p, float t, float scale) {
    float i = floor(p);
    float f = fract(p);
    // Derivative of smoothstep: 6f(1-f)
    float df = 6.0 * f * (1.0 - f);
    float h0 = hash2(vec2(i, floor(t)));
    float h1 = hash2(vec2(i + 1.0, floor(t)));
    return (h1 - h0) * df * scale;
}

// Add 1D smooth interpolation
float smooth_hash1(float p) {
    float i = floor(p);
    float f = fract(p);
    f = f * f * (3.0 - 2.0 * f);
    return mix(hash1(i), hash1(i + 1.0), f);
}

// Calculate wave offset for a given position along an edge
float calc_wave_offset(float pos, float t, float wave_fade) {
    float wave_offset = 0.0;
    // Lower frequencies (base shape)
    wave_offset += sin(pos * 8.0 + t * 0.2) * 0.035;
    wave_offset += sin(pos * 12.0 + t * 0.4) * 0.025;
    // Mid frequencies
    wave_offset += sin(pos * 18.0 - t * 0.5) * 0.018;
    wave_offset += sin(pos * 25.0 - t * 0.7) * 0.015;
    wave_offset += sin(pos * 32.0 + t * 0.9) * 0.012;
    // Higher frequencies (finer detail)
    wave_offset += sin(pos * 45.0 + t * 1.2) * 0.008;
    wave_offset += sin(pos * 60.0 - t * 1.5) * 0.005;
    wave_offset += sin(pos * 80.0 + t * 1.8) * 0.003;
    // Smooth noise using C1-continuous interpolation (replaces discrete hash)
    wave_offset += (smooth_hash(pos * 30.0, t * 0.5) - 0.5) * 0.015;
    wave_offset += (smooth_hash(pos * 50.0, t * 0.7) - 0.5) * 0.008;
    return wave_offset * wave_fade;
}


// Calculate the instantaneous "velocity" of the burn front
// Uses the time derivative of the wave offset (how fast the edge is moving)
float calc_burn_velocity(float pos, float t, float wave_fade) {
    // Derivative of wave offset with respect to time (∂wave/∂t)
    float velocity = 0.0;
    
    // d/dt[sin(pos * freq + t * speed)] = speed * cos(pos * freq + t * speed)
    velocity += 0.2 * cos(pos * 8.0 + t * 0.2) * 0.035;
    velocity += 0.4 * cos(pos * 12.0 + t * 0.4) * 0.025;
    velocity += -0.5 * cos(pos * 18.0 - t * 0.5) * 0.018;
    velocity += -0.7 * cos(pos * 25.0 - t * 0.7) * 0.015;
    velocity += 0.9 * cos(pos * 32.0 + t * 0.9) * 0.012;
    velocity += 1.2 * cos(pos * 45.0 + t * 1.2) * 0.008;
    velocity += -1.5 * cos(pos * 60.0 - t * 1.5) * 0.005;
    velocity += 1.8 * cos(pos * 80.0 + t * 1.8) * 0.003;
    
    // Time derivative of smooth noise
    float noise_vel1 = (smooth_hash(pos * 30.0, t * 0.5 + 0.1) - smooth_hash(pos * 30.0, t * 0.5)) * 0.5 * 0.015;
    float noise_vel2 = (smooth_hash(pos * 50.0, t * 0.7 + 0.1) - smooth_hash(pos * 50.0, t * 0.7)) * 0.7 * 0.008;
    velocity += noise_vel1 + noise_vel2;
    
    // AMPLIFY the velocity significantly (was returning ~0.01-0.05, now ~0.1-0.5)
    return abs(velocity) * wave_fade * 10.0;
}

// Map burn velocity to heat color (faster = hotter = more red/white)
vec3 velocity_heat_color(vec3 base_color, float velocity, float proximity) {
    // Normalize velocity to 0-1 range with wider sensitivity
    // velocity now ranges ~0.0 to 0.5 after amplification
    float heat = smoothstep(0.0, 0.4, velocity);
    
    // Heat color gradient: normal -> orange -> red-hot -> white-hot
    vec3 orange_hot = vec3(1.0, 0.5, 0.0);
    vec3 red_hot = vec3(1.0, 0.2, 0.05);
    vec3 white_hot = vec3(0.5, 0.0, 0.0);
    
    // More aggressive blending through heat stages
    vec3 heat_color = base_color;
    heat_color = mix(heat_color, orange_hot, smoothstep(0.0, 0.25, heat));
    heat_color = mix(heat_color, red_hot, smoothstep(0.2, 0.5, heat));
    heat_color = mix(heat_color, white_hot, smoothstep(0.5, 0.9, heat));
    
    // Apply heat color near the burn edge with stronger effect
    // proximity is already 0-1, boost it slightly
    float boosted_proximity = smoothstep(0.0, 0.7, proximity);
    
    return mix(base_color, heat_color, boosted_proximity);
}
void main()
{
    int burn_side=4;
    float width = size.x;
    float height = size.y;
    
    float burn_progress = progress;
    if (direction == 1) burn_progress = 1.0 - burn_progress;
    
    float t = burn_progress * flame_speed * 10.0;
    
    // ============ WAVY BURN LINE - ALL SIDES ============
    float wave_fade = smoothstep(0.0, 0.1, progress) * smoothstep(1.0, 0.85, progress);
    
    // Calculate effective progress from each side
    // For single side: progress goes 0 to 1 (full burn)
    // For all sides: progress goes 0 to 0.5 (meeting at center)
    float side_progress = (burn_side == 4) ? progress * 0.5 : progress;
    
    // Bottom edge (burning upward)
    float wave_bottom = calc_wave_offset(uvpos.x, t, wave_fade);
    float effective_bottom = (burn_side == 0 || burn_side == 4) ? side_progress + wave_bottom : 0.0;
    float dist_from_bottom = uvpos.y - effective_bottom;
    
    // Top edge (burning downward)
    float wave_top = calc_wave_offset(uvpos.x, t + 5.0, wave_fade);
    float effective_top = (burn_side == 1 || burn_side == 4) ? 1.0 - side_progress - wave_top : 1.0;
    float dist_from_top = effective_top - uvpos.y;
    
    // Left edge (burning rightward)
    float wave_left = calc_wave_offset(uvpos.y, t + 10.0, wave_fade);
    float effective_left = (burn_side == 2 || burn_side == 4) ? side_progress + wave_left : 0.0;
    float dist_from_left = uvpos.x - effective_left;
    
    // Right edge (burning leftward)
    float wave_right = calc_wave_offset(uvpos.y, t + 15.0, wave_fade);
    float effective_right = (burn_side == 3 || burn_side == 4) ? 1.0 - side_progress - wave_right : 1.0;
    float dist_from_right = effective_right - uvpos.x;
    
    // Check if pixel is inside the unburned area (inside all four boundaries)
    bool inside_unburned = (uvpos.y >= effective_bottom && uvpos.y <= effective_top &&
                            uvpos.x >= effective_left && uvpos.x <= effective_right);
    
    // Find the minimum distance (closest burn edge) - only for active sides
    float dist_from_burn = 1000.0;
    if (burn_side == 0 || burn_side == 4) {
        dist_from_burn = min(dist_from_burn, dist_from_bottom);
    }
    if (burn_side == 1 || burn_side == 4) {
        dist_from_burn = min(dist_from_burn, dist_from_top);
    }
    if (burn_side == 2 || burn_side == 4) {
        dist_from_burn = min(dist_from_burn, dist_from_left);
    }
    if (burn_side == 3 || burn_side == 4) {
        dist_from_burn = min(dist_from_burn, dist_from_right);
    }

    // ============ EARLY EXIT OPTIMIZATION ============
    if (dist_from_burn > 0.5) {
        // Far inside unburned area - just show background
        vec4 bg = get_pixel(uvpos);
        gl_FragColor = bg;
        return;
    }
    if (dist_from_burn < -0.05) {
        // Already burned - transparent/black
        gl_FragColor = vec4(0.0);
        return;
    }

    // End blur factor - increases blur near the end of animation
    float end_blur = smoothstep(0.75, 0.95, progress);

    // ============ EDGE-FOLLOWING HEAT DISTORTION ============
    // Distortion follows each edge's curvature
    vec2 total_distort = vec2(0.0);
    float distort_fade = smoothstep(0.0, 0.1, progress) * smoothstep(1.0, 0.85, progress);
    
    // Bottom edge distortion - follows wave_bottom curvature
    if (burn_side == 0 || burn_side == 4) {
        float edge_proximity = smoothstep(0.15, 0.0, dist_from_bottom) * smoothstep(-0.02, 0.02, dist_from_bottom);
        // Derivative of wave gives us the tangent direction
// Enhanced (includes all frequency derivatives + noise derivative)
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
        // Distort perpendicular to edge (upward) with wave influence
        float flicker = 0.8 + 0.2 * sin(t * 8.0 + uvpos.x * 30.0);
        total_distort.y += edge_proximity * 0.015 * flicker;
        total_distort.x += edge_proximity * wave_deriv * 0.3 * flicker;
    }
    
    // Top edge distortion
    if (burn_side == 1 || burn_side == 4) {
        float edge_proximity = smoothstep(0.15, 0.0, dist_from_top) * smoothstep(-0.02, 0.02, dist_from_top);
// Enhanced (includes all frequency derivatives + noise derivative)
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
    
    // Left edge distortion
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
    
    // Right edge distortion
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
    
    // Apply distortion fade
    total_distort *= distort_fade;
    
    // Add high-frequency shimmer on top
    float heat_zone = smoothstep(0.12, 0.0, dist_from_burn) * smoothstep(-0.02, 0.02, dist_from_burn);
    heat_zone *= distort_fade;
    total_distort += vec2(
        sin(uvpos.y * 50.0 + t * 8.0) * heat_zone * 0.008,
        cos(uvpos.x * 45.0 + t * 7.0) * heat_zone * 0.008
    );
    
    vec2 distort_uv = clamp(uvpos + total_distort, 0.0, 1.0);
    
    vec4 bg = get_pixel(distort_uv);
    
    // Hide burned areas (outside the unburned rectangle)
    if (!inside_unburned) {
        bg = vec4(0.0);
    }
    
    // ====== FIRE SHADOW/SMOKE - adds contrast against light backgrounds ======
    if (inside_unburned) {
        // Create a dark shadow zone that extends slightly beyond the fire
        float shadow_zone = smoothstep(0.15, 0.0, dist_from_burn);
        
        // Shadow intensity based on proximity
        float shadow_intensity = shadow_zone * 0.6;
        shadow_intensity = clamp(shadow_intensity, 0.0, 0.85);
        
        // Shadow timing - matches fire fade in/out
        float shadow_delay = 0.05;
        float shadow_fade_duration = 0.15;
        float shadow_progress_factor;
        if (direction == 1) {
            // Reverse: shadow fades out as fire disappears
            //shadow_progress_factor = smoothstep(shadow_delay + shadow_fade_duration, shadow_delay, 1.0 - progress);
        shadow_progress_factor = smoothstep(shadow_delay, shadow_delay + shadow_fade_duration, progress);
        } else {
            // Forward: shadow fades in with fire
            shadow_progress_factor = smoothstep(shadow_delay, shadow_delay + shadow_fade_duration, progress);
        }
        
        // Also fade out near the end of the burn
        shadow_progress_factor *= smoothstep(1.0, 0.85, progress);
        
        // Dark smoke/shadow color (very dark gray with slight warm tint)
        vec3 shadow_color = vec3(0.05, 0.03, 0.02);
        
        // Apply shadow underneath the fire - darkens the background
        bg.rgb = mix(bg.rgb, shadow_color, shadow_intensity * 1.7 * shadow_progress_factor);
    }    

    vec4 result = bg;
    
    // ============ BURN LINE - ALL SIDES ============
    // Only render burn lines on the boundary of the unburned area
    float burn_size_base = 4.0 * (1.0 + end_blur * 2.0);  // Expand burn line at end for blur
    
    vec3 burn_line_total = vec3(0.0);
    float burn_alpha_total = 0.0;
    
    // Pre-compute common values for burn lines
    float progress_fade = clamp(progress * 10.0, 0.0, 1.0);
    float inv_aspect = width / height;
    
// Bottom edge burn line - only if within horizontal bounds of unburned area
if ((burn_side == 0 || burn_side == 4) && uvpos.x >= effective_left && uvpos.x <= effective_right) {
    float edge_dist = dist_from_bottom;
    float edge_pos = uvpos.x;
    
    // Calculate burn velocity at this position
    float burn_velocity = calc_burn_velocity(edge_pos, t, wave_fade);
    
    // Varying thickness for fire-like appearance
    float thickness_var = 0.6 + 0.4 * sin(edge_pos * 15.0 + t * 1.5)
                        + 0.3 * sin(edge_pos * 35.0 - t * 2.3)
                        + 0.2 * sin(edge_pos * 70.0 + t * 3.7)
                        + 0.15 * (smooth_hash(edge_pos * 20.0, t * 0.8) - 0.5);
    thickness_var = clamp(thickness_var, 0.4, 1.4);
    float burn_size = burn_size_base * thickness_var;
    
    float wave = sin(edge_pos * 40.0 + t * 2.0) * 0.003 
               + sin(edge_pos * 80.0 - t * 3.0) * 0.002;
    float edge_noise = smooth_hash(edge_pos * width * 0.5, t * 2.0) * 0.01;
    float adjusted_dist = edge_dist + wave + edge_noise;
    
    float abs_adj = abs(adjusted_dist);
    float ember_core = smoothstep(0.012 * burn_size, 0.0, abs_adj) 
                     * smoothstep(-0.005 * burn_size, 0.005 * burn_size, adjusted_dist);
    float inner_glow = smoothstep(0.025 * burn_size, 0.0, abs_adj) 
                     * smoothstep(-0.01 * burn_size, 0.01 * burn_size, adjusted_dist);
    float outer_glow = smoothstep(0.05 * burn_size, 0.0, abs_adj) 
                     * smoothstep(-0.02 * burn_size, 0.02 * burn_size, adjusted_dist);
    
    // Proximity to burn edge (for heat color application)
    float edge_proximity = smoothstep(0.08, 0.0, abs_adj);
    
    // Orange ember particles
    float ember_particle = 0.0;
    for (float i = 0.0; i < 3.0; i += 1.0) {
        float px = smooth_hash1(i + t * 0.5) * (effective_right - effective_left) + effective_left;
        float local_wave = calc_wave_offset(px, t, wave_fade);
        float py = effective_bottom + sin(t * (2.0 + i) + i * 3.14159) * 0.015 * burn_size;
        float spark_dist = length(vec2((uvpos.x - px) * inv_aspect, uvpos.y - py));
        ember_particle += smoothstep(0.02 * burn_size, 0.0, spark_dist) * (0.5 + 0.5 * sin(t * 10.0 + i * 5.0));
    }
    
    // Red ember particles - different timing and positions
    float red_ember_particle = 0.0;
    for (float i = 0.0; i < 4.0; i += 1.0) {
        float px = smooth_hash1(i + t * 0.4 + 100.0) * (effective_right - effective_left) + effective_left;
        float py = effective_bottom + sin(t * (1.5 + i * 0.7) + i * 2.718) * 0.018 * burn_size;
        float spark_dist = length(vec2((uvpos.x - px) * inv_aspect, uvpos.y - py));
        red_ember_particle += smoothstep(0.025 * burn_size, 0.0, spark_dist) * (0.4 + 0.6 * sin(t * 8.0 + i * 4.0));
    }
    
    // Base burn line colors
    vec3 base_outer = vec3(0.8, 0.2, 0.0);
    vec3 base_inner = vec3(1.0, 0.6, 0.1);
    vec3 base_core = vec3(1.0, 0.95, 0.7);
    vec3 base_ember = vec3(1.0, 0.5, 0.0);
    vec3 base_red_ember = vec3(1.0, 0.1, 0.0);
    
    // Apply velocity-based heat coloring
    vec3 hot_outer = velocity_heat_color(base_outer, burn_velocity, edge_proximity);
    vec3 hot_inner = velocity_heat_color(base_inner, burn_velocity, edge_proximity);
    vec3 hot_core = velocity_heat_color(base_core, burn_velocity * 1.5, edge_proximity);  // Core heats up more
    
    vec3 burn_line = hot_outer * outer_glow 
                   + hot_inner * inner_glow 
                   + hot_core * ember_core 
                   + base_ember * ember_particle
                   + base_red_ember * red_ember_particle;
                   
    float burn_alpha = max(max(outer_glow, inner_glow), max(ember_core, max(ember_particle * 0.8, red_ember_particle * 0.9)));
    burn_alpha *= (0.9 + 0.2 * sin(t * 20.0 + edge_pos * 50.0) * sin(t * 17.0));
    burn_alpha *= progress_fade;
    
    burn_line_total += burn_line * burn_alpha;
    burn_alpha_total = max(burn_alpha_total, burn_alpha);
}
    
// Top edge burn line - only if within horizontal bounds of unburned area
if ((burn_side == 1 || burn_side == 4) && uvpos.x >= effective_left && uvpos.x <= effective_right) {
    float edge_dist = dist_from_top;
    float edge_pos = uvpos.x;
    
    // Calculate burn velocity at this position (with time offset for top edge)
    float burn_velocity = calc_burn_velocity(edge_pos, t + 5.0, wave_fade);
    
    // Varying thickness for fire-like appearance
    float thickness_var = 0.6 + 0.4 * sin(edge_pos * 15.0 + t * 1.5 + 3.0)
                        + 0.3 * sin(edge_pos * 35.0 - t * 2.3 + 5.0)
                        + 0.2 * sin(edge_pos * 70.0 + t * 3.7 + 7.0)
                        + 0.15 * (smooth_hash(edge_pos * 20.0 + 50.0, t * 0.8) - 0.5);
    thickness_var = clamp(thickness_var, 0.4, 1.4);
    float burn_size = burn_size_base * thickness_var;
    
    float wave = sin(edge_pos * 40.0 + t * 2.0 + 5.0) * 0.003 
               + sin(edge_pos * 80.0 - t * 3.0 + 7.0) * 0.002;
    float edge_noise = smooth_hash(edge_pos * width * 0.5, t * 2.0 + 1.0) * 0.01;
    float adjusted_dist = edge_dist + wave + edge_noise;
    
    float abs_adj = abs(adjusted_dist);
    float ember_core = smoothstep(0.012 * burn_size, 0.0, abs_adj) 
                     * smoothstep(-0.005 * burn_size, 0.005 * burn_size, adjusted_dist);
    float inner_glow = smoothstep(0.025 * burn_size, 0.0, abs_adj) 
                     * smoothstep(-0.01 * burn_size, 0.01 * burn_size, adjusted_dist);
    float outer_glow = smoothstep(0.05 * burn_size, 0.0, abs_adj) 
                     * smoothstep(-0.02 * burn_size, 0.02 * burn_size, adjusted_dist);
    
    // Proximity to burn edge (for heat color application)
    float edge_proximity = smoothstep(0.08, 0.0, abs_adj);
    
    // Orange ember particles
    float ember_particle = 0.0;
    for (float i = 0.0; i < 3.0; i += 1.0) {
        float px = smooth_hash1(i + t * 0.5 + 10.0) * (effective_right - effective_left) + effective_left;
        float py = effective_top + sin(t * (2.0 + i) + i * 3.14159 + 2.0) * 0.015 * burn_size;
        float spark_dist = length(vec2((uvpos.x - px) * inv_aspect, uvpos.y - py));
        ember_particle += smoothstep(0.02 * burn_size, 0.0, spark_dist) * (0.5 + 0.5 * sin(t * 10.0 + i * 5.0 + 1.0));
    }
    
    // Red ember particles - different timing and positions
    float red_ember_particle = 0.0;
    for (float i = 0.0; i < 4.0; i += 1.0) {
        float px = smooth_hash1(i + t * 0.4 + 110.0) * (effective_right - effective_left) + effective_left;
        float py = effective_top + sin(t * (1.5 + i * 0.7) + i * 2.718 + 1.5) * 0.018 * burn_size;
        float spark_dist = length(vec2((uvpos.x - px) * inv_aspect, uvpos.y - py));
        red_ember_particle += smoothstep(0.025 * burn_size, 0.0, spark_dist) * (0.4 + 0.6 * sin(t * 8.0 + i * 4.0 + 1.0));
    }
    
    // Base burn line colors
    vec3 base_outer = vec3(0.8, 0.2, 0.0);
    vec3 base_inner = vec3(1.0, 0.6, 0.1);
    vec3 base_core = vec3(1.0, 0.95, 0.7);
    vec3 base_ember = vec3(1.0, 0.5, 0.0);
    vec3 base_red_ember = vec3(1.0, 0.1, 0.0);
    
    // Apply velocity-based heat coloring
    vec3 hot_outer = velocity_heat_color(base_outer, burn_velocity, edge_proximity);
    vec3 hot_inner = velocity_heat_color(base_inner, burn_velocity, edge_proximity);
    vec3 hot_core = velocity_heat_color(base_core, burn_velocity * 1.5, edge_proximity);
    
    vec3 burn_line = hot_outer * outer_glow 
                   + hot_inner * inner_glow 
                   + hot_core * ember_core 
                   + base_ember * ember_particle
                   + base_red_ember * red_ember_particle;
                   
    float burn_alpha = max(max(outer_glow, inner_glow), max(ember_core, max(ember_particle * 0.8, red_ember_particle * 0.9)));
    burn_alpha *= (0.9 + 0.2 * sin(t * 20.0 + edge_pos * 50.0 + 3.0) * sin(t * 17.0));
    burn_alpha *= progress_fade;
    
    burn_line_total += burn_line * burn_alpha;
    burn_alpha_total = max(burn_alpha_total, burn_alpha);
}
    
// Left edge burn line - only if within vertical bounds of unburned area
if ((burn_side == 2 || burn_side == 4) && uvpos.y >= effective_bottom && uvpos.y <= effective_top) {
    float edge_dist = dist_from_left;
    float edge_pos = uvpos.y;
    
    // Calculate burn velocity at this position (with time offset for left edge)
    float burn_velocity = calc_burn_velocity(edge_pos, t + 10.0, wave_fade);
    
    // Varying thickness for fire-like appearance
    float thickness_var = 0.6 + 0.4 * sin(edge_pos * 15.0 + t * 1.5 + 6.0)
                        + 0.3 * sin(edge_pos * 35.0 - t * 2.3 + 10.0)
                        + 0.2 * sin(edge_pos * 70.0 + t * 3.7 + 14.0)
                        + 0.15 * (smooth_hash(edge_pos * 20.0 + 100.0, t * 0.8) - 0.5);
    thickness_var = clamp(thickness_var, 0.4, 1.4);
    float burn_size = burn_size_base * thickness_var;
    
    float wave = sin(edge_pos * 40.0 + t * 2.0 + 10.0) * 0.003 
               + sin(edge_pos * 80.0 - t * 3.0 + 14.0) * 0.002;
    float edge_noise = smooth_hash(edge_pos * height * 0.5, t * 2.0 + 2.0) * 0.01;
    float adjusted_dist = edge_dist + wave + edge_noise;
    
    float abs_adj = abs(adjusted_dist);
    float ember_core = smoothstep(0.012 * burn_size, 0.0, abs_adj) 
                     * smoothstep(-0.005 * burn_size, 0.005 * burn_size, adjusted_dist);
    float inner_glow = smoothstep(0.025 * burn_size, 0.0, abs_adj) 
                     * smoothstep(-0.01 * burn_size, 0.01 * burn_size, adjusted_dist);
    float outer_glow = smoothstep(0.05 * burn_size, 0.0, abs_adj) 
                     * smoothstep(-0.02 * burn_size, 0.02 * burn_size, adjusted_dist);
    
    // Proximity to burn edge (for heat color application)
    float edge_proximity = smoothstep(0.08, 0.0, abs_adj);
    
    // Orange ember particles
    float ember_particle = 0.0;
    for (float i = 0.0; i < 3.0; i += 1.0) {
        float py = smooth_hash1(i + t * 0.5 + 20.0) * (effective_top - effective_bottom) + effective_bottom;
        float px = effective_left + sin(t * (2.0 + i) + i * 3.14159 + 4.0) * 0.015 * burn_size;
        float spark_dist = length(vec2((uvpos.x - px) * inv_aspect, uvpos.y - py));
        ember_particle += smoothstep(0.02 * burn_size, 0.0, spark_dist) * (0.5 + 0.5 * sin(t * 10.0 + i * 5.0 + 2.0));
    }
    
    // Red ember particles - different timing and positions
    float red_ember_particle = 0.0;
    for (float i = 0.0; i < 4.0; i += 1.0) {
        float py = smooth_hash1(i + t * 0.4 + 120.0) * (effective_top - effective_bottom) + effective_bottom;
        float px = effective_left + sin(t * (1.5 + i * 0.7) + i * 2.718 + 3.0) * 0.018 * burn_size;
        float spark_dist = length(vec2((uvpos.x - px) * inv_aspect, uvpos.y - py));
        red_ember_particle += smoothstep(0.025 * burn_size, 0.0, spark_dist) * (0.4 + 0.6 * sin(t * 8.0 + i * 4.0 + 2.0));
    }
    
    // Base burn line colors
    vec3 base_outer = vec3(0.8, 0.2, 0.0);
    vec3 base_inner = vec3(1.0, 0.6, 0.1);
    vec3 base_core = vec3(1.0, 0.95, 0.7);
    vec3 base_ember = vec3(1.0, 0.5, 0.0);
    vec3 base_red_ember = vec3(1.0, 0.1, 0.0);
    
    // Apply velocity-based heat coloring
    vec3 hot_outer = velocity_heat_color(base_outer, burn_velocity, edge_proximity);
    vec3 hot_inner = velocity_heat_color(base_inner, burn_velocity, edge_proximity);
    vec3 hot_core = velocity_heat_color(base_core, burn_velocity * 1.5, edge_proximity);
    
    vec3 burn_line = hot_outer * outer_glow 
                   + hot_inner * inner_glow 
                   + hot_core * ember_core 
                   + base_ember * ember_particle
                   + base_red_ember * red_ember_particle;
                   
    float burn_alpha = max(max(outer_glow, inner_glow), max(ember_core, max(ember_particle * 0.8, red_ember_particle * 0.9)));
    burn_alpha *= (0.9 + 0.2 * sin(t * 20.0 + edge_pos * 50.0 + 6.0) * sin(t * 17.0));
    burn_alpha *= progress_fade;
    
    burn_line_total += burn_line * burn_alpha;
    burn_alpha_total = max(burn_alpha_total, burn_alpha);
}
    
// Right edge burn line - only if within vertical bounds of unburned area
if ((burn_side == 3 || burn_side == 4) && uvpos.y >= effective_bottom && uvpos.y <= effective_top) {
    float edge_dist = dist_from_right;
    float edge_pos = uvpos.y;
    
    // Calculate burn velocity at this position (with time offset for right edge)
    float burn_velocity = calc_burn_velocity(edge_pos, t + 15.0, wave_fade);
    
    // Varying thickness for fire-like appearance
    float thickness_var = 0.6 + 0.4 * sin(edge_pos * 15.0 + t * 1.5 + 9.0)
                        + 0.3 * sin(edge_pos * 35.0 - t * 2.3 + 15.0)
                        + 0.2 * sin(edge_pos * 70.0 + t * 3.7 + 21.0)
                        + 0.15 * (smooth_hash(edge_pos * 20.0 + 150.0, t * 0.8) - 0.5);
    thickness_var = clamp(thickness_var, 0.4, 1.4);
    float burn_size = burn_size_base * thickness_var;
    
    float wave = sin(edge_pos * 40.0 + t * 2.0 + 15.0) * 0.003 
               + sin(edge_pos * 80.0 - t * 3.0 + 21.0) * 0.002;
    float edge_noise = smooth_hash(edge_pos * height * 0.5, t * 2.0 + 3.0) * 0.01;
    float adjusted_dist = edge_dist + wave + edge_noise;
    
    float abs_adj = abs(adjusted_dist);
    float ember_core = smoothstep(0.012 * burn_size, 0.0, abs_adj) 
                     * smoothstep(-0.005 * burn_size, 0.005 * burn_size, adjusted_dist);
    float inner_glow = smoothstep(0.025 * burn_size, 0.0, abs_adj) 
                     * smoothstep(-0.01 * burn_size, 0.01 * burn_size, adjusted_dist);
    float outer_glow = smoothstep(0.05 * burn_size, 0.0, abs_adj) 
                     * smoothstep(-0.02 * burn_size, 0.02 * burn_size, adjusted_dist);
    
    // Proximity to burn edge (for heat color application)
    float edge_proximity = smoothstep(0.08, 0.0, abs_adj);
    
    // Orange ember particles
    float ember_particle = 0.0;
    for (float i = 0.0; i < 3.0; i += 1.0) {
        float py = smooth_hash1(i + t * 0.5 + 30.0) * (effective_top - effective_bottom) + effective_bottom;
        float px = effective_right + sin(t * (2.0 + i) + i * 3.14159 + 6.0) * 0.015 * burn_size;
        float spark_dist = length(vec2((uvpos.x - px) * inv_aspect, uvpos.y - py));
        ember_particle += smoothstep(0.02 * burn_size, 0.0, spark_dist) * (0.5 + 0.5 * sin(t * 10.0 + i * 5.0 + 3.0));
    }
    
    // Red ember particles - different timing and positions
    float red_ember_particle = 0.0;
    for (float i = 0.0; i < 4.0; i += 1.0) {
        float py = smooth_hash1(i + t * 0.4 + 130.0) * (effective_top - effective_bottom) + effective_bottom;
        float px = effective_right + sin(t * (1.5 + i * 0.7) + i * 2.718 + 4.5) * 0.018 * burn_size;
        float spark_dist = length(vec2((uvpos.x - px) * inv_aspect, uvpos.y - py));
        red_ember_particle += smoothstep(0.025 * burn_size, 0.0, spark_dist) * (0.4 + 0.6 * sin(t * 8.0 + i * 4.0 + 3.0));
    }
    
    // Base burn line colors
    vec3 base_outer = vec3(0.8, 0.2, 0.0);
    vec3 base_inner = vec3(1.0, 0.6, 0.1);
    vec3 base_core = vec3(1.0, 0.95, 0.7);
    vec3 base_ember = vec3(1.0, 0.5, 0.0);
    vec3 base_red_ember = vec3(1.0, 0.1, 0.0);
    
    // Apply velocity-based heat coloring
    vec3 hot_outer = velocity_heat_color(base_outer, burn_velocity, edge_proximity);
    vec3 hot_inner = velocity_heat_color(base_inner, burn_velocity, edge_proximity);
    vec3 hot_core = velocity_heat_color(base_core, burn_velocity * 1.5, edge_proximity);
    
    vec3 burn_line = hot_outer * outer_glow 
                   + hot_inner * inner_glow 
                   + hot_core * ember_core 
                   + base_ember * ember_particle
                   + base_red_ember * red_ember_particle;
                   
    float burn_alpha = max(max(outer_glow, inner_glow), max(ember_core, max(ember_particle * 0.8, red_ember_particle * 0.9)));
    burn_alpha *= (0.9 + 0.2 * sin(t * 20.0 + edge_pos * 50.0 + 9.0) * sin(t * 17.0));
    burn_alpha *= progress_fade;
    
    burn_line_total += burn_line * burn_alpha;
    burn_alpha_total = max(burn_alpha_total, burn_alpha);
}
    
    result.rgb += burn_line_total * (1.0 - end_blur * 0.7);  // Fade burn lines at end
    
    // Final blur softening - reduce contrast at the end
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
