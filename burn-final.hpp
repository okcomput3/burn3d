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



// 3D noise for volumetric flame simulation
vec3 hash3_flame(vec3 p) {
    p = vec3(
        dot(p, vec3(127.1, 311.7, 74.7)),
        dot(p, vec3(269.5, 183.3, 246.1)),
        dot(p, vec3(113.5, 271.9, 124.6))
    );
    return -1.0 + 2.0 * fract(sin(p) * 43758.5453123);
}

float noise3D_flame(vec3 p) {
    vec3 i = floor(p);
    vec3 f = fract(p);
    // Quintic interpolation for smoother diffusion
    vec3 u = f * f * f * (f * (f * 6.0 - 15.0) + 10.0);
    
    return mix(
        mix(mix(dot(hash3_flame(i + vec3(0,0,0)), f - vec3(0,0,0)),
                dot(hash3_flame(i + vec3(1,0,0)), f - vec3(1,0,0)), u.x),
            mix(dot(hash3_flame(i + vec3(0,1,0)), f - vec3(0,1,0)),
                dot(hash3_flame(i + vec3(1,1,0)), f - vec3(1,1,0)), u.x), u.y),
        mix(mix(dot(hash3_flame(i + vec3(0,0,1)), f - vec3(0,0,1)),
                dot(hash3_flame(i + vec3(1,0,1)), f - vec3(1,0,1)), u.x),
            mix(dot(hash3_flame(i + vec3(0,1,1)), f - vec3(0,1,1)),
                dot(hash3_flame(i + vec3(1,1,1)), f - vec3(1,1,1)), u.x), u.y),
        u.z
    );
}

// Velocity field simulating rising, turbulent gas (Navier-Stokes inspired)
vec2 flameVelocityField(vec2 p, float time, float turbulence_strength) {
    float riseSpeed = 2.0;
    // Vortex motion (swirling flames)
    float vortex1 = sin(p.y * 3.0 + time * 2.0) * cos(p.x * 2.5 + time);
    float vortex2 = cos(p.y * 2.0 - time * 1.5) * sin(p.x * 3.0 - time * 0.7);
    // Diffusion spreads particles as they rise
    float spread = (1.0 - p.y) * 0.3;
    float diffusionX = noise3D_flame(vec3(p * 2.0, time * 0.5)) * spread;
    
    return vec2(
        (vortex1 + vortex2) * 0.15 * turbulence_strength + diffusionX,
        riseSpeed
    );
}

// FBM with velocity field advection (particles follow streamlines)
float advectedFBM_flame(vec2 p, float time, float speed, float turbulence) {
    float value = 0.0;
    float amplitude = 0.5;
    float frequency = 1.0;
    vec2 pos = p;
    
    // Advect through velocity field
    for(int i = 0; i < 3; i++) {
        vec2 vel = flameVelocityField(pos, time, turbulence);
        pos += vel * 0.02;
    }
    
    // Multi-octave noise
    for(int i = 0; i < 6; i++) {
        vec3 samplePos = vec3(pos * frequency, time * speed * 0.8);
        value += amplitude * noise3D_flame(samplePos);
        
        pos += vec2(
            noise3D_flame(vec3(pos * frequency * 0.5, time * 0.3)) * 0.1,
            noise3D_flame(vec3(pos * frequency * 0.5 + 100.0, time * 0.3)) * 0.05
        ) * turbulence;
        
        amplitude *= 0.5;
        frequency *= 2.0;
    }
    return value;
}

// Temperature-based flame color (lecture: yellow=hot, red=cool)
vec3 temperatureToFlameColor(float temperature, float density, float intensity) {
    vec3 hotCore = vec3(1.0, 0.95, 0.7);    // Yellow-white
    vec3 midFlame = vec3(1.0, 0.5, 0.0);    // Orange  
    vec3 coolEdge = vec3(0.8, 0.15, 0.0);   // Deep red
    vec3 smoke = vec3(0.1, 0.05, 0.02);     // Dark embers
    
    vec3 color;
    if(temperature > 0.7) {
        color = mix(midFlame, hotCore, (temperature - 0.7) / 0.3);
    } else if(temperature > 0.4) {
        color = mix(coolEdge, midFlame, (temperature - 0.4) / 0.3);
    } else {
        color = mix(smoke, coolEdge, temperature / 0.4);
    }
    
    return color * (1.0 + density * intensity);
}

// === FBM NOISE FOR SIMPLE FLAME ===
vec2 hash_vec2(vec2 p) {
    p = vec2(dot(p, vec2(127.1, 311.7)),
             dot(p, vec2(269.5, 183.3)));
    return -1.0 + 2.0 * fract(sin(p) * 43758.5453123);
}

float simplex_noise(in vec2 p) {
    const float K1 = 0.366025404;
    const float K2 = 0.211324865;
    vec2 i = floor(p + (p.x + p.y) * K1);
    vec2 a = p - i + (i.x + i.y) * K2;
    vec2 o = (a.x > a.y) ? vec2(1.0, 0.0) : vec2(0.0, 1.0);
    vec2 b = a - o + K2;
    vec2 c = a - 1.0 + 2.0 * K2;
    vec3 h = max(0.5 - vec3(dot(a, a), dot(b, b), dot(c, c)), 0.0);
    vec3 n = h * h * h * h * vec3(dot(a, hash_vec2(i)), dot(b, hash_vec2(i + o)), dot(c, hash_vec2(i + 1.0)));
    return dot(n, vec3(70.0));
}

float fbm(vec2 uv) {
    float f;
    mat2 m = mat2(1.7, 1.2, -1.2, 1.7);
    f = 0.5000 * simplex_noise(uv); uv = m * uv;
    f += 0.2500 * simplex_noise(uv); uv = m * uv;
    f += 0.1250 * simplex_noise(uv); uv = m * uv;
    f += 0.0625 * simplex_noise(uv);
    return 0.5 + 0.3 * f;
}

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

// IMPROVED: Curl noise for swirling vortex motion
// From lecture: "using fluid dynamics... to create effects such as swirling and vortices"
float curl_noise(vec2 p, float t) {
    float eps = 0.01;
    float n1 = smooth_noise2D(p + vec2(eps, 0.0) + vec2(0.0, t * 0.3));
    float n2 = smooth_noise2D(p - vec2(eps, 0.0) + vec2(0.0, t * 0.3));
    float n3 = smooth_noise2D(p + vec2(0.0, eps) + vec2(0.0, t * 0.3));
    float n4 = smooth_noise2D(p - vec2(0.0, eps) + vec2(0.0, t * 0.3));
    // Curl is perpendicular to gradient - creates rotational motion
    return (n3 - n4) - (n1 - n2);
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

// Convert RGB to HSV
vec3 rgb2hsv(vec3 c) {
    vec4 K = vec4(0.0, -1.0/3.0, 2.0/3.0, -1.0);
    vec4 p = mix(vec4(c.bg, K.wz), vec4(c.gb, K.xy), step(c.b, c.g));
    vec4 q = mix(vec4(p.xyw, c.r), vec4(c.r, p.yzx), step(p.x, c.r));
    float d = q.x - min(q.w, q.y);
    float e = 1.0e-10;
    return vec3(abs(q.z + (q.w - q.y) / (6.0 * d + e)), d / (q.x + e), q.x);
}

// Convert HSV to RGB
vec3 hsv2rgb(vec3 c) {
    vec4 K = vec4(1.0, 2.0/3.0, 1.0/3.0, 3.0);
    vec3 p = abs(fract(c.xxx + K.xyz) * 6.0 - K.www);
    return c.z * mix(K.xxx, clamp(p - K.xxx, 0.0, 1.0), c.y);
}

// Apply flame_color hue to a base color
// If flame_color is white/unsaturated, keep original colors
vec3 apply_flame_hue(vec3 base_color) {
    vec3 flame_hsv = rgb2hsv(flame_color.rgb);
    // If flame_color has low saturation (white/grey), return original color
    if (flame_hsv.y < 0.1) {
        return base_color;
    }
    vec3 base_hsv = rgb2hsv(base_color);
    // Replace hue with flame_color's hue, keep original saturation and value
    return hsv2rgb(vec3(flame_hsv.x, base_hsv.y, base_hsv.z));
}

// IMPROVED: Temperature-based color with proper blackbody radiation approximation
// From lecture: "The colouring successfully fools the eye... Yellow represents the hottest, cooling to red"
vec3 temperature_to_color(float temp) {
    // temp: 0.0 = cool (dark), 1.0 = hottest (white-yellow)
    // Based on blackbody radiation: hot -> white/blue, cooling -> yellow -> orange -> red -> dark
    vec3 color;
    if (temp > 0.9) {
        // Hottest: white with slight blue tinge (oxygen-rich combustion)
        color = mix(vec3(1.0, 0.95, 0.8), vec3(0.9, 0.95, 1.0), (temp - 0.9) * 10.0);
    } else if (temp > 0.7) {
        // Very hot: bright yellow-white
        color = mix(vec3(1.0, 0.85, 0.3), vec3(1.0, 0.95, 0.8), (temp - 0.7) * 5.0);
    } else if (temp > 0.5) {
        // Hot: yellow-orange
        color = mix(vec3(1.0, 0.5, 0.1), vec3(1.0, 0.85, 0.3), (temp - 0.5) * 5.0);
    } else if (temp > 0.3) {
        // Medium: orange-red
        color = mix(vec3(0.8, 0.2, 0.05), vec3(1.0, 0.5, 0.1), (temp - 0.3) * 5.0);
    } else if (temp > 0.1) {
        // Cool: dark red
        color = mix(vec3(0.3, 0.05, 0.02), vec3(0.8, 0.2, 0.05), (temp - 0.1) * 5.0);
    } else {
        // Coolest: ember glow to black
        color = mix(vec3(0.0), vec3(0.3, 0.05, 0.02), temp * 10.0);
    }
    return apply_flame_hue(color);
}



// IMPROVED: Rising ash with viscous drag and buoyancy
// From lecture: "Burning gases are lighter than air and so tend to rise"
// Also: "f = ma + sv" where s is viscous drag coefficient
float calc_rising_ash(vec2 uv, float edge_start, float edge_end, float burn_edge, float t, float inv_aspect, bool is_vertical) {
    float ash_particles = 0.0;
    for (float i = 0.0; i < 8.0; i += 1.0) {
        float spawn_pos = hash1(i + floor(t * 0.15) * 7.0 + burn_edge * 100.0) * (edge_end - edge_start) + edge_start;
        float life = fract(t * 0.1 + i * 0.125);
        
        // IMPROVED: Buoyancy with viscous drag (terminal velocity behavior)
        // From lecture: damping creates f = ma + sv, leading to terminal velocity
        float drag_coeff = 3.0;
        float buoyancy = 0.25;
        // Velocity approaches terminal velocity: v_terminal = buoyancy / drag
        // Position integrates to: x = v_terminal * t - (v_terminal/drag) * (1 - e^(-drag*t))
        float effective_time = life * 2.0;
        float rise_height = (buoyancy / drag_coeff) * effective_time * (1.0 - exp(-drag_coeff * effective_time * 0.5));
        
        // Horizontal drift from curl noise (swirling motion)
        float curl = curl_noise(vec2(spawn_pos * 5.0, life * 3.0), t);
        float drift = curl * 0.04 * life;
        
        // Add some random wobble
        drift += sin(life * 8.0 + i * 2.5) * 0.02 * life;
        
        float px, py;
        if (is_vertical) {
            px = burn_edge + rise_height + drift;
            py = spawn_pos;
        } else {
            px = spawn_pos + drift;
            py = burn_edge + rise_height;
        }
        
        // Ash size decreases as it cools and breaks apart
        float ash_size = 0.014 * (1.0 - life * 0.7);
        float ash_dist = length(vec2((uv.x - px) * inv_aspect, uv.y - py));
        
        // Flickering glow that fades as ember cools
        float glow_flicker = 0.5 + 0.5 * sin(t * 6.0 + i * 4.0) * (1.0 - life);
        float ash_alpha = smoothstep(ash_size, 0.0, ash_dist) * (1.0 - life * life) * glow_flicker;
        
        ash_particles += ash_alpha;
    }
    return ash_particles * 0.6;
}


// IMPROVED: Sparks that follow parabolic paths with drag
// From lecture: "Simple equations determine the path of each particle"
// Using f = ma + sv (Newtonian with viscous drag)
vec3 calc_spark_particle(vec2 uv, float spawn_x, float spawn_y, float t, float seed, float inv_aspect) {
    float life = fract(t * 0.15 + seed * 0.1);
    
    // Initial velocity (randomized ejection)
    float angle = hash1(seed * 17.3) * 3.14159 * 0.5 + 0.25; // Upward bias
    float speed = 0.3 + hash1(seed * 23.7) * 0.2;
    vec2 v0 = vec2(cos(angle), sin(angle)) * speed;
    
    // Physics: position with drag and gravity
    // From lecture: vt+1 = at*dt + vt, with damping f = ma + sv
    float drag = 2.5;
    float gravity = -0.15;
    float effective_t = life * 1.5;
    
    // Approximate solution with drag: x = v0/drag * (1 - e^(-drag*t)) + 0.5*g*t^2 (simplified)
    float exp_term = 1.0 - exp(-drag * effective_t);
    vec2 pos = vec2(
        spawn_x + v0.x / drag * exp_term,
        spawn_y + v0.y / drag * exp_term + 0.5 * gravity * effective_t * effective_t
    );
    
    // Spark size and brightness decrease with life (cooling)
    float spark_size = 0.008 * (1.0 - life * 0.8);
    float spark_dist = length(vec2((uv.x - pos.x) * inv_aspect, uv.y - pos.y));
    
    float spark_alpha = smoothstep(spark_size, 0.0, spark_dist);
    
    // Temperature decreases with life
    float temp = 1.0 - life * 0.7;
    vec3 spark_color = temperature_to_color(temp);
    
    // Flickering
    spark_alpha *= 0.7 + 0.3 * sin(t * 15.0 + seed * 10.0);
    spark_alpha *= (1.0 - life * life); // Fade out
    
    return spark_color * spark_alpha;
}

void main()
{
    int burn_side = 0;
    float width = size.x;
    float height = size.y;

        // Early escape - don't process outside window for single-side burns
    if (burn_side != 4) {
        if (uvpos.x < 0.0 || uvpos.x > 1.0 || uvpos.y < 0.0 || uvpos.y > 1.0) {
            gl_FragColor = vec4(0.0);
            return;
        }
    }
    
    float burn_progress = progress;
    if (direction == 1) burn_progress = 1.0 - burn_progress;
    
    float t = burn_progress * flame_speed * 10.0;
    
    float wave_fade = smoothstep(0.00, 0.05, progress) * smoothstep(1.00, 0.95, progress);
float side_progress = (burn_side == 4 || burn_side == 5 || burn_side == 6) ? progress * 0.5 : progress;




// Paper fiber at this pixel (affects burn edge shape)
float fiber = paper_fiber(uvpos, t);
    

    
    // Fiber-based offset: thin areas (low fiber) burn faster/further
    float fiber_offset = (fiber - 0.5) * 0.025 * wave_fade;
    
    // Bottom edge
    float wave_bottom = calc_wave_offset(uvpos.x, t, wave_fade) *0.3;
    float effective_bottom = (burn_side == 0 || burn_side == 5 || burn_side == 6) ? side_progress + wave_bottom + fiber_offset : 0.0;
    float dist_from_bottom = uvpos.y - effective_bottom;
    
    // Top edge
    float wave_top = calc_wave_offset(uvpos.x, t + 5.0, wave_fade);
    float effective_top = (burn_side == 1 || burn_side == 5|| burn_side == 6) ? 1.0 - side_progress - wave_top - fiber_offset : 1.0;
    float dist_from_top = effective_top - uvpos.y;
    
    // Left edge
    float wave_left = calc_wave_offset(uvpos.y, t + 10.0, wave_fade);
    float effective_left = (burn_side == 2 || burn_side == 4|| burn_side == 6) ? side_progress + wave_left + fiber_offset : 0.0;
    float dist_from_left = uvpos.x - effective_left;
    
    // Right edge
    float wave_right = calc_wave_offset(uvpos.y, t + 15.0, wave_fade);
    float effective_right = (burn_side == 3 || burn_side == 4|| burn_side == 6) ? 1.0 - side_progress - wave_right - fiber_offset : 1.0;
    float dist_from_right = effective_right - uvpos.x;
    
    bool inside_unburned = (uvpos.y >= effective_bottom && uvpos.y <= effective_top &&
                            uvpos.x >= effective_left && uvpos.x <= effective_right);


                       
    
    float dist_from_burn = 1000.0;
    if (burn_side == 0 || burn_side == 5 || burn_side == 6) dist_from_burn = min(dist_from_burn, dist_from_bottom);
    if (burn_side == 1 || burn_side == 5 || burn_side == 6) dist_from_burn = min(dist_from_burn, dist_from_top);
    if (burn_side == 2 || burn_side == 4 || burn_side == 6) dist_from_burn = min(dist_from_burn, dist_from_left);
    if (burn_side == 3 || burn_side == 4 || burn_side == 6) dist_from_burn = min(dist_from_burn, dist_from_right);

// Early exit for far pixels (unburned area far from edge)
    if (dist_from_burn > 0.5) {
        gl_FragColor = get_pixel(uvpos);
        return;
    }
    
    // For deeply burned areas, start with transparent and only render ash/effects
    bool in_ash_zone = dist_from_burn < -0.08;

    float end_blur = smoothstep(0.75, 0.95, progress);
    float distort_fade = smoothstep(0.00, 0.05, progress) * smoothstep(1.00, 0.95, progress);
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
    
    if (burn_side == 0 || burn_side == 5) {
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
    
    if (burn_side == 1 || burn_side == 5) {
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
    
// === IMPROVED HEAT SHIMMER - Rising effect with curl noise ===
// From lecture: burning gases rise due to being lighter than air
float heat_zone = smoothstep(0.48, 0.0, dist_from_burn) * smoothstep(-0.02, 0.02, dist_from_burn);
heat_zone *= distort_fade;

float shimmer_speed = 0.3; // Lower = slower (try 0.1 - 0.5)

// IMPROVED: Curl noise for swirling heat distortion
float curl = curl_noise(uvpos * 12.0, t * 0.5 * shimmer_speed);

// Standard shimmer with curl-based swirl
total_distort += vec2(
    sin(uvpos.y * 50.0 + t * 8.0 * shimmer_speed) * heat_zone * 0.008 + curl * heat_zone * 0.006,
    cos(uvpos.x * 45.0 + t * 7.0 * shimmer_speed) * heat_zone * 0.008
);

// Rising heat shimmer - heat rises more than it moves sideways
float rising_shimmer = sin(uvpos.x * 60.0 + t * 4.0 * shimmer_speed - uvpos.y * 30.0) * heat_zone * 0.006;
rising_shimmer += sin(uvpos.x * 90.0 + t * 6.0 * shimmer_speed - uvpos.y * 45.0) * heat_zone * 0.004;
total_distort.y += rising_shimmer;
    
    vec2 distort_uv = clamp(uvpos + total_distort, 0.0, 1.0);
    vec4 bg = get_pixel(distort_uv);
    
    if (!inside_unburned) {
        bg = vec4(0.0);
    }
    
  

    vec4 result = bg;
    

        if (in_ash_zone) {
        result = vec4(0.0);
    }
    float burn_size_base = 2.0 * (1.0 + end_blur * 2.0);
    vec3 burn_line_total = vec3(0.0);
    float burn_alpha_total = 0.0;
    float progress_fade = clamp(progress * 10.0, 0.0, 1.0);

    // Turbulence for flame flickering
    float flame_turb = turbulence(vec2(uvpos.x * 15.0, uvpos.y * 15.0 + t * 0.5), t);

    
    // === IMPROVED SMOKE WISPS with diffusion ===
    // From lecture: fire creates smoke that rises and diffuses

// === FALLING ASH - Realistic charred flakes ===
    {
        float ash_grid = 20.0; // Fewer, larger pieces
        vec3 ash_result = vec3(0.0);
        float ash_alpha_total = 0.0;
        
        vec2 grid_cell = floor(uvpos * ash_grid);
        
        // Check 5x5 neighborhood for larger particles
        for (float ox = -2.0; ox <= 2.0; ox += 1.0) {
            for (float oy = -2.0; oy <= 2.0; oy += 1.0) {
                vec2 cell = grid_cell + vec2(ox, oy);
                
                if (cell.x < 0.0 || cell.x >= ash_grid || cell.y < 0.0 || cell.y >= ash_grid) continue;
                
                float cell_id = cell.x + cell.y * ash_grid;
                float seed = cell_id * 17.3;
                
                vec2 original_pos = (cell + vec2(hash1(seed), hash1(seed + 100.0))) / ash_grid;
                
                // Calculate local burn line
                float local_wave = calc_wave_offset(original_pos.x, t, wave_fade) * 0.1;
                float local_fiber = paper_fiber(original_pos, t);
                float local_fiber_offset = (local_fiber - 0.5) * 0.025 * wave_fade;
                float local_burn_line = side_progress + local_wave + local_fiber_offset;
                
                float time_since_burn = (local_burn_line - original_pos.y);
                
                if (time_since_burn < 0.0) continue;
                
                float life = clamp(time_since_burn * 1.2, 0.0, 1.0);
                
                // === SLOWER, DRIFTING FALL (like real ash) ===
                float drag = 1.0 + hash1(seed + 300.0) * 3.0; // More drag = slower
                float gravity = 0.04; // Lighter
                float v_terminal = gravity / drag;
                float fall_time = life * 5.0;
                float fall_dist = v_terminal * fall_time;
                
                // === FLUTTERING DRIFT ===
                float flutter_freq = 2.0 + hash1(seed + 400.0) * 2.0;
                float flutter_amp = 0.06 + hash1(seed + 450.0) * 0.04;
                float flutter = sin(life * flutter_freq * 6.28 + seed) * flutter_amp * (1.0 - life * 0.3);
                
                // Curl noise for organic movement
                float curl_x = curl_noise(original_pos * 4.0 + vec2(life * 1.5, 0.0), t * 0.3);
                float curl_drift = curl_x * 0.05 * life;
                
                float total_drift = flutter + curl_drift;
                
                // === WAVE CONTINUES ===
                float ash_wave = calc_wave_offset(original_pos.x + total_drift, t, wave_fade) * 0.1;
                float wave_influence = (1.0 - life * 0.8);
                
                // === FINAL POSITION ===
                vec2 ash_pos = vec2(
                    original_pos.x + total_drift,
                    original_pos.y - fall_dist + ash_wave * wave_influence
                );
                
                if (ash_pos.y < -0.2 || ash_pos.y > 1.2) continue;
                if (ash_pos.x < -0.2 || ash_pos.x > 1.2) continue;
                
                // === IRREGULAR FLAKE SHAPE ===
                // Base size - much larger
                float base_size = (0.5 / ash_grid) * (0.7 + hash1(seed + 600.0) * 0.6);
                
                // Tumbling rotation
                float rotation = life * (3.0 + hash1(seed + 700.0) * 4.0) + seed;
                
                // Apparent size changes as flake tumbles (thin edge vs flat face)
                float tumble_phase = sin(rotation) * 0.5 + 0.5;
                float size_x = base_size * (0.4 + tumble_phase * 0.6);
                float size_y = base_size * (0.6 + (1.0 - tumble_phase) * 0.4);
                
                // Shrink over life as edges crumble
                size_x *= (1.0 - life * 0.5);
                size_y *= (1.0 - life * 0.5);
                
                // === DISTANCE WITH ROTATION ===
                vec2 delta = vec2((uvpos.x - ash_pos.x) * inv_aspect, uvpos.y - ash_pos.y);
                
                // Rotate delta
                float rot_angle = rotation * 0.5;
                float cs = cos(rot_angle);
                float sn = sin(rot_angle);
                vec2 rotated_delta = vec2(
                    delta.x * cs - delta.y * sn,
                    delta.x * sn + delta.y * cs
                );
                
                // Elliptical distance for flake shape
                float dist = length(vec2(rotated_delta.x / size_x, rotated_delta.y / size_y));
                
                // === IRREGULAR EDGES (torn paper) ===
                float angle = atan(rotated_delta.y, rotated_delta.x);
                float edge_tear = 0.0;
                edge_tear += sin(angle * 3.0 + seed * 1.1) * 0.15;
                edge_tear += sin(angle * 5.0 + seed * 2.3) * 0.1;
                edge_tear += sin(angle * 8.0 + seed * 3.7) * 0.08;
                edge_tear += sin(angle * 13.0 + seed * 5.1) * 0.05;
                
                // More torn as it ages
                edge_tear *= (1.0 + life * 0.5);
                
                float particle_alpha = smoothstep(1.0 + edge_tear, 0.6 + edge_tear * 0.5, dist);
                
                // === SAMPLE WINDOW (for texture reference) ===
                vec2 sample_uv = clamp(original_pos, 0.0, 1.0);
                vec4 window_sample = get_pixel(sample_uv);
                
                // === ASH COLOR - Charred, gray, desaturated ===
// === ASH COLOR - Grey with realistic variation ===
                // Base grey value
                float grey_base = 0.25;
                
                // Per-particle variation (some flakes lighter/darker)
                float grey_variation = hash1(seed + 800.0) * 0.15 - 0.075;
                
                // Variation across the flake surface
                float surface_variation = hash1(seed + floor(angle * 3.0) * 50.0) * 0.1 - 0.05;
                
                // Combine into base grey
                vec3 ash_color = vec3(grey_base + grey_variation + surface_variation);
                
                // Darken over life (ash gets darker as it cools/chars)
                ash_color *= (1.0 - life * 0.4);
                
                // Add subtle warm/cool variation (some ash slightly brownish, some bluish-grey)
                float temp_variation = step(0.5, hash1(seed + 900.0));
                vec3 warm_tint = vec3(1.05, 1.0, 0.95);
                vec3 cool_tint = vec3(0.95, 0.98, 1.05);
                ash_color *= mix(cool_tint, warm_tint, temp_variation);
                
                // Edge darkening (edges burn more)
                float edge_darken = smoothstep(0.3, 0.9, dist) * 0.15;
                ash_color *= (1.0 - edge_darken);
                
                // === SUBTLE EMBER GLOW (only at very edges, early in life) ===
                float ember_zone = smoothstep(0.9, 1.0 + edge_tear, dist) * (1.0 - smoothstep(0.0, 0.4, life));
                vec3 ember_col = vec3(0.8, 0.2, 0.05) * ember_zone * 0.3;
                ember_col *= 0.7 + 0.3 * sin(t * 8.0 + seed * 5.0);
                ash_color += ember_col;
                
                // === ALPHA ===
                float alpha_fade = (1.0 - life * life * 0.8) * window_sample.a;
                
                // Random holes/breaks appear
                float break_noise = hash1(seed + floor(life * 6.0) * 100.0 + floor(angle * 2.0) * 30.0);
                if (break_noise < life * 0.3) alpha_fade *= 0.1;
                
                particle_alpha *= alpha_fade;
                
                // Accumulate with proper blending
                float blend_factor = particle_alpha * (1.0 - ash_alpha_total * 0.5);
                ash_result = mix(ash_result, ash_color, blend_factor);
                ash_alpha_total = max(ash_alpha_total, particle_alpha * 0.9);
            }
        }
        
        // Blend ash over result
        result.rgb = mix(result.rgb, ash_result, ash_alpha_total);
        result.a = max(result.a, ash_alpha_total);
    }


    // === SIMPLE FLAME OVERLAY ===
{
    // Apply distortion using existing smooth_noise2D function
    float distortion_scale = 8.0; // tweak this value as needed
    vec2 distorted_uvpos = uvpos + (vec2(
        smooth_noise2D(uvpos * distortion_scale + t / distortion_scale),
        smooth_noise2D(uvpos * distortion_scale - t / distortion_scale)) *
        0.1 * (1.0 - burn_progress) - (1.0 - burn_progress) * 0.05);

    vec2 flame_uv = vec2(distorted_uvpos.x * width * 0.02, (distorted_uvpos.y - progress) * height * 0.002);
    vec2 q = vec2(flame_uv.x, flame_uv.y);
    q.x *= (1.05 - flame_width) * 3.0;
    q.y *= pow(0.4 / ((flame_height * 0.3) + 0.1), clamp(distorted_uvpos.y + 0.2 - progress, 0.1, 0.7) * 20.0);

    float T3 = burn_progress * flame_speed;
    q.x -= 0.2;
    q.y -= 0.1;
    float n = fbm(q - vec2(0.0, T3));
    float c = 1.0 - 12.0 * pow(max(0.0, length(vec2(q.x * 0.0001, q.y) * vec2(1.8 + q.y * 1.5, 0.75)) - n * max(0.0, q.y + 0.25)), 1.2);
    float c1 = n * c * (1.5 - pow(1.25 * flame_uv.y, 4.0));
    c = clamp(c, 0.0, 1.0);
    c1 = clamp(c1, 0.0, 1.0);

    float r = 1.0 / flame_color.r;
    float g = 1.0 / flame_color.g;
    float b = 1.0 / flame_color.b;
    vec3 simple_flame_col = vec3(1.5 * pow(c1, r), 1.5 * pow(c1, g), 1.5 * pow(c1, b));

    float simple_a = clamp(c * (1.0 - pow(distorted_uvpos.y, 10.0)), 0.0, 1.0);
    simple_a *= clamp(progress * 100.0, 0.0, 1.0);
    simple_a *= clamp(distorted_uvpos.x * (width / 30.0), 0.0, 1.0);
    simple_a *= clamp((1.0 - distorted_uvpos.x) * (width / 30.0), 0.0, 1.0);

    result.rgb = mix(result.rgb, simple_flame_col, simple_a);
    result.a = max(result.a, simple_a);
}
  




 // === LAYERED REALISTIC FLAME OVERLAY ===
{
    vec3 total_flame = vec3(0.0);
    float total_alpha = 0.0;
    
    // 5 layers: back to front, each with different characteristics
    for (int layer = 0; layer < 6; layer++) {

 if (layer == 0) continue;
  if (layer == 1) continue;
   if (layer == 2) continue;
    if (layer == 5) continue;
     if (layer == 6) continue;
        float layer_f = float(layer);
        bool is_last_layer = (layer == 4);
        
        // Layer properties - back layers are larger/slower, front are smaller/faster
        float distortion_scale = 2.0 + layer_f * 2.0;
        float layer_speed = 0.85 + layer_f * 0.075;
        float layer_width = 3.5 - layer_f * 0.25;
        float layer_intensity = 0.35 + layer_f * 0.15;
        
        // Last layer sits lower to cover seams
        float layer_offset = is_last_layer ? -0.03 : layer_f * 0.008;
        
        // Distortion - back layers have more gentle distortion
        float distort_amount = 0.1 * (1.0 - burn_progress);
        float distort_dampen = 1.0 - layer_f * 0.1;
        
        vec2 distorted_uvpos = uvpos + vec2(
            smooth_noise2D(uvpos * distortion_scale + t / distortion_scale + layer_f * 10.0),
            smooth_noise2D(uvpos * distortion_scale - t / distortion_scale + layer_f * 10.0)
        ) * distort_amount * distort_dampen - distort_amount * 0.5 * distort_dampen;
        
        // Flame UV with layer offset
        vec2 flame_uv = vec2(
            distorted_uvpos.x * width * 0.02,
            (distorted_uvpos.y - progress - layer_offset) * height * 0.002
        );
        
        vec2 q = flame_uv;
        
        // Last layer is wider at the base to cover seams
        float base_width = is_last_layer ? layer_width * 1.3 : layer_width;
        q.x *= (1.05 - flame_width) * base_width;
        
        // Height scaling - very small difference between layers
        float height_scale = 1.0 + (4.0 - layer_f) * 0.02;
        q.y *= pow(0.4 / ((flame_height * 0.3 * height_scale) + 0.1), 
                   clamp(distorted_uvpos.y + 0.2 - progress, 0.1, 0.7) * 20.0);
        
        // Noise sampling with layer-specific time offset
        float T3 = burn_progress * flame_speed * layer_speed;
        q.x -= 0.2;
        q.y -= 0.1;
        
        // Add layer-specific noise offset for variety
        float n = fbm(q - vec2(layer_f * 0.3, T3));
        
        // Shape calculation
        float c = 1.0 - 12.0 * pow(
            max(0.0, length(vec2(q.x * 0.0001, q.y) * vec2(1.8 + q.y * 1.5, 0.75)) - n * max(0.0, q.y + 0.25)),
            1.2
        );
        float c1 = n * c * (1.5 - pow(1.25 * flame_uv.y, 4.0));
        c1 = max(c1, 0.0);  
        c = clamp(c, 0.0, 1.0);
        c1 = clamp(c1, 0.0, 1.0);
        
        // Color - back layers are redder/darker, front layers are yellow/white
        float color_shift = layer_f / 4.0;
        vec3 layer_tint = mix(vec3(1.0, 0.4, 0.15), vec3(1.0, 0.85, 0.6), color_shift);
        
        float r = 1.0 / (flame_color.r * layer_tint.r);
        float g = 1.0 / (flame_color.g * layer_tint.g);
        float b = 1.0 / (flame_color.b * layer_tint.b);
        
        vec3 flame_col = vec3(
            1.5 * pow(c1, r),
            1.5 * pow(c1, g),
            1.5 * pow(c1, b)
        );
        
        // Add subtle flickering per layer
        float flicker = 0.9 + 0.1 * sin(t * (8.0 + layer_f * 2.0) + layer_f * 2.0);
        flame_col *= flicker;
        
        // Alpha calculation - last layer is more opaque at the base
        float alpha = clamp(c * (1.0 - pow(distorted_uvpos.y, 10.0)), 0.0, 1.0);
        alpha *= clamp(progress * 100.0, 0.0, 1.0);
        alpha *= clamp(distorted_uvpos.x * (width / 30.0), 0.0, 1.0);
        alpha *= clamp((1.0 - distorted_uvpos.x) * (width / 30.0), 0.0, 1.0);
        
        // Boost last layer intensity at the base to hide seams
        float base_boost = is_last_layer ? 1.5 : 1.0;
        alpha *= layer_intensity * base_boost;
        
        // Blend layers back-to-front
        float blend_mode = layer_f / 4.0;
        
        // Last layer uses more opaque blending
        if (is_last_layer) {
            blend_mode = 0.85;  // More opaque "over" blend
        }
        
        // Additive component (glow from back layers)
        total_flame += flame_col * alpha * (1.0 - blend_mode) * 0.5;
        
        // Over blend component (front layers cover)
        total_flame = mix(total_flame, flame_col, alpha * blend_mode);
        
        total_alpha = max(total_alpha, alpha);
    }
    
    // Apply combined flame
    result.rgb = mix(result.rgb, total_flame, total_alpha);
    result.a = max(result.a, total_alpha);
}
 // === LAYERED REALISTIC FLAME OVERLAY ===
{
    vec3 total_flame = vec3(0.0);
    float total_alpha = 0.0;
    
    // 5 layers: back to front, each with different characteristics
    for (int layer = 0; layer < 6; layer++) {

 if (layer == 0) continue;
  if (layer == 1) continue;
   if (layer == 2) continue;
    if (layer == 3) continue;
     if (layer == 4) continue;
        float layer_f = float(layer);
        bool is_last_layer = (layer == 5);
        
        // Layer properties - back layers are larger/slower, front are smaller/faster
        float distortion_scale = 2.0 + layer_f * 2.0;
        float layer_speed = 0.85 + layer_f * 0.075;
        float layer_width = 3.5 - layer_f * 0.25;
        float layer_intensity = 0.35 + layer_f * 0.15;
        
        // Last layer sits lower to cover seams
        float layer_offset = is_last_layer ? -0.03 : layer_f * 0.008;
        
        // Distortion - back layers have more gentle distortion
        float distort_amount = 0.1 * (1.0 - burn_progress);
        float distort_dampen = 1.0 - layer_f * 0.1;
        
        vec2 distorted_uvpos = uvpos + vec2(
            smooth_noise2D(uvpos * distortion_scale + t / distortion_scale + layer_f * 10.0),
            smooth_noise2D(uvpos * distortion_scale - t / distortion_scale + layer_f * 10.0)
        ) * distort_amount * distort_dampen - distort_amount * 0.5 * distort_dampen;
        
        // Flame UV with layer offset
        vec2 flame_uv = vec2(
            distorted_uvpos.x * width * 0.02,
            (distorted_uvpos.y - progress - layer_offset) * height * 0.002
        );
        
        vec2 q = flame_uv;
        
        // Last layer is wider at the base to cover seams
        float base_width = is_last_layer ? layer_width * 1.3 : layer_width;
        q.x *= (1.05 - flame_width) * base_width;
        
        // Height scaling - very small difference between layers
        float height_scale = 1.0 + (4.0 - layer_f) * 0.02;
        q.y *= pow(0.4 / ((flame_height * 0.3 * height_scale) + 0.1), 
                   clamp(distorted_uvpos.y + 0.2 - progress, 0.1, 0.7) * 20.0);
        
        // Noise sampling with layer-specific time offset
        float T3 = burn_progress * flame_speed * layer_speed;
        q.x -= 0.2;
        q.y -= 0.1;
        
        // Add layer-specific noise offset for variety
        float n = fbm(q - vec2(layer_f * 0.3, T3));
        
        // Shape calculation
        float c = 1.0 - 12.0 * pow(
            max(0.0, length(vec2(q.x * 0.0001, q.y) * vec2(1.8 + q.y * 1.5, 0.75)) - n * max(0.0, q.y + 0.25)),
            1.2
        );
        float c1 = n * c * (1.5 - pow(1.25 * flame_uv.y, 4.0));
        c1 = max(c1, 0.0);  
        c = clamp(c, 0.0, 1.0);
        c1 = clamp(c1, 0.0, 1.0);
        
        // Color - back layers are redder/darker, front layers are yellow/white
        float color_shift = layer_f / 4.0;
        vec3 layer_tint = mix(vec3(1.0, 0.4, 0.15), vec3(1.0, 0.85, 0.6), color_shift);
        
        float r = 1.0 / (flame_color.r * layer_tint.r);
        float g = 1.0 / (flame_color.g * layer_tint.g);
        float b = 1.0 / (flame_color.b * layer_tint.b);
        
        vec3 flame_col = vec3(
            1.5 * pow(c1, r),
            1.5 * pow(c1, g),
            1.5 * pow(c1, b)
        );
        
        // Add subtle flickering per layer
        float flicker = 0.9 + 0.1 * sin(t * (8.0 + layer_f * 2.0) + layer_f * 2.0);
        flame_col *= flicker;
        
        // Alpha calculation - last layer is more opaque at the base
        float alpha = clamp(c * (1.0 - pow(distorted_uvpos.y, 10.0)), 0.0, 1.0);
        alpha *= clamp(progress * 100.0, 0.0, 1.0);
        alpha *= clamp(distorted_uvpos.x * (width / 30.0), 0.0, 1.0);
        alpha *= clamp((1.0 - distorted_uvpos.x) * (width / 30.0), 0.0, 1.0);
        
        // Boost last layer intensity at the base to hide seams
        float base_boost = is_last_layer ? 1.5 : 1.0;
        alpha *= layer_intensity * base_boost;
        
        // Blend layers back-to-front
        float blend_mode = layer_f / 4.0;
        
        // Last layer uses more opaque blending
        if (is_last_layer) {
            blend_mode = 0.85;  // More opaque "over" blend
        }
        
        // Additive component (glow from back layers)
        total_flame += flame_col * alpha * (1.0 - blend_mode) * 0.5;
        
        // Over blend component (front layers cover)
        total_flame = mix(total_flame, flame_col, alpha * blend_mode);
        
        total_alpha = max(total_alpha, alpha);
    }
    
    // Apply combined flame
    result.rgb = mix(result.rgb, total_flame, total_alpha);
    result.a = max(result.a, total_alpha);
}


// === LAYERED REALISTIC FLAME OVERLAY ===
{
    vec3 total_flame = vec3(0.0);
    float total_alpha = 0.0;
    
    // Layers 3, 4, 5 only
    for (int layer = 5; layer < 6; layer++) {
        float layer_f = float(layer);
        bool is_last_layer = (layer == 5);
        
        // Layer properties - back layers are larger/slower, front are smaller/faster
        float distortion_scale = 2.0 + layer_f * 2.0;
        float layer_speed = 0.85 + layer_f * 0.075;
        float layer_width = 3.5 - layer_f * 0.25;
        float layer_intensity = 0.35 + layer_f * 0.15;
        
        // Last layer sits lower to cover seams
        float layer_offset = is_last_layer ? -0.03 : layer_f * 0.008;
        
        // Distortion - back layers have more gentle distortion
        float distort_amount = 0.1 * (1.0 - burn_progress);
        float distort_dampen = 1.0 - layer_f * 0.1;
        
        vec2 distorted_uvpos = uvpos + vec2(
            smooth_noise2D(uvpos * distortion_scale + t / distortion_scale + layer_f * 10.0),
            smooth_noise2D(uvpos * distortion_scale - t / distortion_scale + layer_f * 10.0)
        ) * distort_amount * distort_dampen - distort_amount * 0.5 * distort_dampen;
        
        // === ADVECT UV THROUGH VELOCITY FIELD ===
        // Simulate particles rising and swirling through flame
        vec2 advected_uv = distorted_uvpos;
        float turbulence_strength = 0.8 + layer_f * 0.1; // More turbulence in front layers
        for (int adv = 0; adv < 3; adv++) {
            vec2 vel = flameVelocityField(advected_uv * 2.0, t * layer_speed, turbulence_strength);
            advected_uv += vel * 0.008 * (1.0 - layer_f * 0.1);
        }
        
        // Flame UV with layer offset - use advected position
        vec2 flame_uv = vec2(
            advected_uv.x * width * 0.02,
            (advected_uv.y - progress - layer_offset) * height * 0.002
        );
        
        vec2 q = flame_uv;
        
        // Last layer is wider at the base to cover seams
        float base_width = is_last_layer ? layer_width * 1.3 : layer_width;
        q.x *= (1.05 - flame_width) * base_width;
        
        // Height scaling - very small difference between layers
        float height_scale = 1.0 + (4.0 - layer_f) * 0.02;
        q.y *= pow(0.4 / ((flame_height * 0.3 * height_scale) + 0.1), 
                   clamp(advected_uv.y + 0.2 - progress, 0.1, 0.7) * 20.0);
        
        // Noise sampling with layer-specific time offset
        float T3 = burn_progress * flame_speed * layer_speed;
        q.x -= 0.2;
        q.y -= 0.1;
        
        // === USE ADVECTED FBM FOR MORE REALISTIC SWIRLING FLAMES ===
        // Blend between regular fbm and advected fbm based on layer
        float n_regular = fbm(q - vec2(layer_f * 0.3, T3));
        float n_advected = advectedFBM_flame(q, t * layer_speed, flame_speed * 0.5, turbulence_strength) * 0.5 + 0.5;
        float advect_blend = 0.3 + layer_f * 0.1; // Front layers use more advected noise
        float n = mix(n_regular, n_advected, advect_blend);
        
        // Shape calculation
        float c = 1.0 - 12.0 * pow(
            max(0.0, length(vec2(q.x * 0.0001, q.y) * vec2(1.8 + q.y * 1.5, 0.75)) - n * max(0.0, q.y + 0.25)),
            1.2
        );
        float c1 = n * c * (1.5 - pow(1.25 * flame_uv.y, 4.0));
        c1 = max(c1, 0.0);  
        c = clamp(c, 0.0, 1.0);
        c1 = clamp(c1, 0.0, 1.0);
        
        // Color - back layers are redder/darker, front layers are yellow/white
        float color_shift = layer_f / 4.0;
        vec3 layer_tint = mix(vec3(1.0, 0.4, 0.15), vec3(1.0, 0.85, 0.6), color_shift);
        
        float r = 1.0 / (flame_color.r * layer_tint.r);
        float g = 1.0 / (flame_color.g * layer_tint.g);
        float b = 1.0 / (flame_color.b * layer_tint.b);
        
        vec3 flame_col = vec3(
            1.5 * pow(c1, r),
            1.5 * pow(c1, g),
            1.5 * pow(c1, b)
        );
        
        // Add subtle flickering per layer
        float flicker = 0.9 + 0.1 * sin(t * (8.0 + layer_f * 2.0) + layer_f * 2.0);
        flame_col *= flicker;
        
        // Alpha calculation - last layer is more opaque at the base
        float alpha = clamp(c * (1.0 - pow(advected_uv.y, 10.0)), 0.0, 1.0);
        alpha *= clamp(progress * 100.0, 0.0, 1.0);
        alpha *= clamp(advected_uv.x * (width / 30.0), 0.0, 1.0);
        alpha *= clamp((1.0 - advected_uv.x) * (width / 30.0), 0.0, 1.0);
        
        // Boost last layer intensity at the base to hide seams
        float base_boost = is_last_layer ? 1.5 : 1.0;
        alpha *= layer_intensity * base_boost;
        
        // Blend layers back-to-front
        float blend_mode = layer_f / 4.0;
        
        // Last layer uses more opaque blending
        if (is_last_layer) {
            blend_mode = 0.85;
        }
        
        // Additive component (glow from back layers)
        total_flame += flame_col * alpha * (1.0 - blend_mode) * 0.5;
        
        // Over blend component (front layers cover)
        total_flame = mix(total_flame, flame_col, alpha * blend_mode);
        
        total_alpha = max(total_alpha, alpha);
    }
    
    // Apply combined flame
    result.rgb = mix(result.rgb, total_flame, total_alpha);
    result.a = max(result.a, total_alpha);
}
// === SPARK PARTICLES ===
    {
        vec3 spark_total = vec3(0.0);
        
        // Spawn sparks along the burn edge
        for (float i = 0.0; i < 100.0; i += 1.0) {
            float seed = i * 7.3 + floor(t * 0.2) * 13.0;
            
            // Spawn position along burn edge
            float spawn_pos = hash1(seed) * 0.8 + 0.1; // Keep away from edges
            float spawn_x, spawn_y;
            
            if (burn_side == 0 || burn_side == 5 || burn_side == 6) {
                // Bottom edge sparks
                spawn_x = spawn_pos;
                spawn_y = effective_bottom;
                spark_total += calc_spark_particle(uvpos, spawn_x, spawn_y, t, seed, inv_aspect);
            }
            if (burn_side == 1 || burn_side == 5 || burn_side == 6) {
                // Top edge sparks
                spawn_x = spawn_pos;
                spawn_y = effective_top;
                spark_total += calc_spark_particle(uvpos, spawn_x, spawn_y, t, seed + 100.0, inv_aspect);
            }
            if (burn_side == 2 || burn_side == 4 || burn_side == 6) {
                // Left edge sparks
                spawn_x = effective_left;
                spawn_y = spawn_pos;
                spark_total += calc_spark_particle(uvpos, spawn_x, spawn_y, t, seed + 200.0, inv_aspect);
            }
            if (burn_side == 3 || burn_side == 4 || burn_side == 6) {
                // Right edge sparks
                spawn_x = effective_right;
                spawn_y = spawn_pos;
                spark_total += calc_spark_particle(uvpos, spawn_x, spawn_y, t, seed + 300.0, inv_aspect);
            }
        }
        
        // Additive blend sparks (they glow)
        result.rgb += spark_total * progress_fade;
        result.a = max(result.a, length(spark_total) * 0.5 * progress_fade);
    }

// === GHOST OVERLAY - Window content over flames at 25% opacity ===
    {
        vec4 ghost_bg = get_pixel(distort_uv);
        if (inside_unburned) {
            // Only overlay in the unburned region (where the window still exists)
            result.rgb = mix(result.rgb, ghost_bg.rgb, 0.35 * ghost_bg.a);
        }
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
