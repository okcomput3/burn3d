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

// Velocity heat color with blue core support
vec3 velocity_heat_color(vec3 base_color, float velocity, float proximity) {
    float heat = smoothstep(0.0, 0.4, velocity);
    
    vec3 orange_hot = apply_flame_hue(vec3(1.0, 0.5, 0.0));
    vec3 red_hot = apply_flame_hue(vec3(1.0, 0.2, 0.05));
    vec3 white_hot = apply_flame_hue(vec3(0.5, 0.0, 0.0));

    
    
    vec3 heat_color = base_color;
    heat_color = mix(heat_color, orange_hot, smoothstep(0.0, 0.25, heat));
    heat_color = mix(heat_color, red_hot, smoothstep(0.2, 0.5, heat));
    heat_color = mix(heat_color, white_hot, smoothstep(0.5, 0.9, heat));
    
    float boosted_proximity = smoothstep(0.0, 0.7, proximity);
    return mix(base_color, heat_color, boosted_proximity);
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

// IMPROVED: Smoke wisps with proper diffusion behavior
// From lecture: "Gases diffuse from more dense regions to less dense ones and cool"
float calc_smoke_wisps(vec2 uv, float dist_from_burn, float t, float progress_fade) {
    float smoke_dist = dist_from_burn - 0.015;
    if (smoke_dist > 0.0 && smoke_dist < 0.35) {
        // Density decreases with distance (diffusion)
        float smoke_density = smoothstep(0.35, 0.0, smoke_dist);
        smoke_density *= smoke_density; // Quadratic falloff for more realistic diffusion
        
        // IMPROVED: Curl noise for swirling smoke motion
        float curl = curl_noise(uv * 8.0, t * 0.3);
        vec2 smoke_uv = uv + vec2(curl * 0.02, -smoke_dist * 0.5);
        
        // Wispy noise pattern - multiple octaves for detail
        float smoke_noise = smooth_noise2D(vec2(smoke_uv.x * 20.0, smoke_uv.y * 15.0 - t * 0.5));
        smoke_noise *= smooth_noise2D(vec2(smoke_uv.x * 40.0 + t * 0.2, smoke_uv.y * 25.0 - t * 0.4));
        smoke_noise += smooth_noise2D(vec2(smoke_uv.x * 70.0 - t * 0.1, smoke_uv.y * 50.0 - t * 0.6)) * 0.5;
        
        // Smoke rises and spreads (diffusion)
        float rise_factor = 1.0 + smoke_dist * 2.0;
        smoke_noise *= rise_factor;
        
        float smoke_alpha = smoke_density * smoke_noise * 0.4;
        smoke_alpha *= progress_fade;
        
        return clamp(smoke_alpha, 0.0, 0.7);
    }
    return 0.0;
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


// Edge fade for single-side burns - fades fire to alpha at perpendicular edges
float edge_fade = 3.0;
if (burn_side == 0 || burn_side == 1) {
    // Horizontal burn (top/bottom) - fade at left and right edges
    float fade_width = 0.08;
    float left_fade = smoothstep(0.0, fade_width, uvpos.x);
    float right_fade = smoothstep(0.0, fade_width, 1.0 - uvpos.x);
    edge_fade = left_fade * right_fade;
} else if (burn_side == 2 || burn_side == 3) {
    // Vertical burn (left/right) - fade at top and bottom edges
    float fade_width = 0.08;
    float bottom_fade = smoothstep(0.0, fade_width, uvpos.y);
    float top_fade = smoothstep(0.0, fade_width, 1.0 - uvpos.y);
    edge_fade = bottom_fade * top_fade;
}

// Paper fiber at this pixel (affects burn edge shape)
float fiber = paper_fiber(uvpos, t);
    

    
    // Fiber-based offset: thin areas (low fiber) burn faster/further
    float fiber_offset = (fiber - 0.5) * 0.025 * wave_fade;
    
    // Bottom edge
    float wave_bottom = calc_wave_offset(uvpos.x, t, wave_fade) *0.1;
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
        
        // === IMPROVED SECONDARY ILLUMINATION (Radiosity-lite) ===
        // From lecture: "fire creates the effect of making the flame illuminate the surrounding objects"
        float illumination_zone = smoothstep(0.35, 0.0, dist_from_burn);
        
        // IMPROVED: More complex flickering based on turbulence
        float flame_turb_illum = turbulence(uvpos * 10.0, t * 0.8);
        float flicker_illum = 0.7 + 0.3 * flame_turb_illum;
        flicker_illum *= 0.85 + 0.15 * sin(t * 17.0 + uvpos.x * 35.0);
        
        // Illumination color varies with distance (hotter closer)
        float illum_temp = 0.8 - dist_from_burn * 3.0;
        vec3 fire_light = temperature_to_color(clamp(illum_temp, 0.3, 0.9)) * illumination_zone * 0.22 * flicker_illum;
        fire_light *= distort_fade;
        // Don't illuminate heavily shadowed areas as much
        bg.rgb += fire_light * (1.0 - shadow_intensity * 0.5);
    }

    vec4 result = bg;
    
    float burn_size_base = 2.0 * (1.0 + end_blur * 2.0);
    vec3 burn_line_total = vec3(0.0);
    float burn_alpha_total = 0.0;
    float progress_fade = clamp(progress * 10.0, 0.0, 1.0);

    // Turbulence for flame flickering
    float flame_turb = turbulence(vec2(uvpos.x * 15.0, uvpos.y * 15.0 + t * 0.5), t);

    // === BOTTOM EDGE ===
    if ((burn_side == 0 || burn_side == 5|| burn_side == 6) && uvpos.x >= effective_left && uvpos.x <= effective_right) {
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
float fiber_edge = (fiber - 0.5) * 0.008 * wave_fade;
float wave = (sin(edge_pos * 40.0 + t * 2.0) * 0.003 + sin(edge_pos * 80.0 - t * 3.0) * 0.002) * wave_fade;
float edge_noise = smooth_hash(edge_pos * width * 0.5, t * 2.0) * 0.01 * wave_fade;
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
        
        // IMPROVED: Sparks with proper physics
        vec3 spark_total = vec3(0.0);
        for (float i = 0.0; i < 200.0; i += 1.0) {
            float spawn_x = smooth_hash1(i + floor(t * 0.3) * 7.0) * (effective_right - effective_left) + effective_left;
            spark_total += calc_spark_particle(uvpos, spawn_x, effective_bottom, t, i + 100.0, inv_aspect);
        }
        
        // Orange ember particles (original style, kept for density)
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
        
        // IMPROVED: Use temperature-based colors for more realistic heat distribution
        // From lecture: "Yellow represents the hottest, cooling to red through the particle life time"
        float core_temp = 0.95 - abs_adj * 8.0;
        float inner_temp = 0.75 - abs_adj * 5.0;
        float outer_temp = 0.5 - abs_adj * 3.0;
        
        vec3 core_color = temperature_to_color(clamp(core_temp, 0.0, 1.0));
        vec3 inner_color = temperature_to_color(clamp(inner_temp, 0.0, 1.0));
        vec3 outer_color = temperature_to_color(clamp(outer_temp, 0.0, 1.0));
        
        vec3 base_ember = apply_flame_hue(vec3(1.0, 0.5, 0.0));
        vec3 base_red_ember = apply_flame_hue(vec3(1.0, 0.1, 0.0));
        vec3 ash_color = vec3(0.15, 0.12, 0.10);
        
        // Blue flame at hottest core (oxygen-rich combustion)
        vec3 blue_flame = apply_flame_hue(vec3(0.3, 0.5, 1.0));
        float blue_core = smoothstep(0.006 * burn_size, 0.0, abs_adj) * 
                          smoothstep(-0.002 * burn_size, 0.002 * burn_size, adjusted_dist);
        
        vec3 hot_outer = velocity_heat_color(outer_color, burn_velocity, edge_proximity);
        vec3 hot_inner = velocity_heat_color(inner_color, burn_velocity, edge_proximity);
        vec3 hot_core = velocity_heat_color(core_color, burn_velocity * 1.5, edge_proximity);
        
        vec3 burn_line = hot_outer * outer_glow 
                       + hot_inner * inner_glow 
                       + hot_core * ember_core 
                       + blue_flame * blue_core * 0.35
                       + base_ember * ember_particle
                       + base_red_ember * red_ember_particle
                       + ash_color * ash_particles
                       + spark_total;
        
        // Charred edge with crack pattern
        float char_zone = smoothstep(0.005, -0.03, edge_dist);
        float char_texture = fiber * 0.4 + 0.6;
        float cracks = crack_pattern(uvpos * 12.0 + vec2(t * 0.05, 0.0), edge_pos * 10.0);
        vec3 char_color = vec3(0.06, 0.03, 0.02) * char_texture;
        char_color = mix(char_color, vec3(0.02, 0.01, 0.005), cracks * 0.6);
        burn_line = mix(burn_line, char_color, char_zone * 0.55);
        
        float burn_alpha = max(max(outer_glow, inner_glow), max(ember_core, max(ember_particle * 0.8, max(red_ember_particle * 0.9, ash_particles * 0.5))));
        burn_alpha = max(burn_alpha, blue_core * 0.4);
        burn_alpha = max(burn_alpha, length(spark_total) * 0.8);
        burn_alpha *= (0.9 + 0.2 * sin(t * 20.0 + edge_pos * 50.0) * sin(t * 17.0));
        burn_alpha *= progress_fade * edge_fade;
        
        burn_line_total += burn_line * burn_alpha;
        burn_alpha_total = max(burn_alpha_total, burn_alpha);
    }

    // === TOP EDGE ===
    if ((burn_side == 1 || burn_side == 5 || burn_side == 6) && uvpos.x >= effective_left && uvpos.x <= effective_right) {
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
        
        float fiber_edge = (fiber - 0.5) * 0.008 * wave_fade;;
        float wave = sin(edge_pos * 40.0 + t * 2.0 + 5.0) * 0.003 + sin(edge_pos * 80.0 - t * 3.0 + 7.0) * 0.002 * wave_fade;;
        float edge_noise = smooth_hash(edge_pos * width * 0.5, t * 2.0 + 1.0) * 0.01* wave_fade;;
        float adjusted_dist = edge_dist + wave + edge_noise + fiber_edge;
        
        float abs_adj = abs(adjusted_dist);
        float turb_mod = 0.7 + 0.3 * flame_turb;
        
        float ember_core = smoothstep(0.012 * burn_size, 0.0, abs_adj) * smoothstep(-0.005 * burn_size, 0.005 * burn_size, adjusted_dist) * turb_mod;
        float inner_glow = smoothstep(0.025 * burn_size, 0.0, abs_adj) * smoothstep(-0.01 * burn_size, 0.01 * burn_size, adjusted_dist) * (0.85 + 0.15 * flame_turb);
        float outer_glow = smoothstep(0.05 * burn_size, 0.0, abs_adj) * smoothstep(-0.02 * burn_size, 0.02 * burn_size, adjusted_dist) * (0.9 + 0.1 * flame_turb);
        float edge_proximity = smoothstep(0.08, 0.0, abs_adj);
        
        // Sparks
        vec3 spark_total = vec3(0.0);
        for (float i = 0.0; i < 5.0; i += 1.0) {
            float spawn_x = smooth_hash1(i + floor(t * 0.3) * 11.0 + 50.0) * (effective_right - effective_left) + effective_left;
            spark_total += calc_spark_particle(uvpos, spawn_x, effective_top, t, i + 200.0, inv_aspect);
        }
        
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
        for (float i = 0.0; i < 6.0; i += 1.0) {
            float spawn_x = hash1(i + floor(t * 0.2) * 11.0) * (effective_right - effective_left) + effective_left;
            float life = fract(t * 0.08 + i * 0.167);
            // Fall with drag
            float drag = 2.0;
            float gravity = 0.2;
            float fall_dist = gravity / drag * life * (1.0 - exp(-drag * life * 2.0));
            float drift = curl_noise(vec2(spawn_x * 5.0, life * 3.0), t + 10.0) * 0.03 * life;
            float px = spawn_x + drift;
            float py = effective_top - fall_dist;
            float ash_size = 0.012 * (1.0 - life * 0.6);
            float ash_dist = length(vec2((uvpos.x - px) * inv_aspect, uvpos.y - py));
            ash_particles += smoothstep(ash_size, 0.0, ash_dist) * (1.0 - life * life) * 0.5;
        }
        
        // Temperature-based colors
        float core_temp = 0.95 - abs_adj * 8.0;
        float inner_temp = 0.75 - abs_adj * 5.0;
        float outer_temp = 0.5 - abs_adj * 3.0;
        
        vec3 core_color = temperature_to_color(clamp(core_temp, 0.0, 1.0));
        vec3 inner_color = temperature_to_color(clamp(inner_temp, 0.0, 1.0));
        vec3 outer_color = temperature_to_color(clamp(outer_temp, 0.0, 1.0));
        
        vec3 base_ember = apply_flame_hue(vec3(1.0, 0.5, 0.0));
        vec3 base_red_ember = apply_flame_hue(vec3(1.0, 0.1, 0.0));
        vec3 ash_color = vec3(0.15, 0.12, 0.10);
        vec3 blue_flame = apply_flame_hue(vec3(0.3, 0.5, 1.0));
        
        float blue_core = smoothstep(0.006 * burn_size, 0.0, abs_adj) * 
                          smoothstep(-0.002 * burn_size, 0.002 * burn_size, adjusted_dist);
        
        vec3 hot_outer = velocity_heat_color(outer_color, burn_velocity, edge_proximity);
        vec3 hot_inner = velocity_heat_color(inner_color, burn_velocity, edge_proximity);
        vec3 hot_core = velocity_heat_color(core_color, burn_velocity * 1.5, edge_proximity);
        
        vec3 burn_line = hot_outer * outer_glow 
                       + hot_inner * inner_glow 
                       + hot_core * ember_core 
                       + blue_flame * blue_core * 0.35
                       + base_ember * ember_particle
                       + base_red_ember * red_ember_particle
                       + ash_color * ash_particles
                       + spark_total;
        
        float char_zone = smoothstep(0.005, -0.03, edge_dist);
        float char_texture = fiber * 0.4 + 0.6;
        float cracks = crack_pattern(uvpos * 12.0 + vec2(t * 0.05, 0.0), edge_pos * 10.0 + 50.0);
        vec3 char_color = vec3(0.06, 0.03, 0.02) * char_texture;
        char_color = mix(char_color, vec3(0.02, 0.01, 0.005), cracks * 0.6);
        burn_line = mix(burn_line, char_color, char_zone * 0.55);
        
        float burn_alpha = max(max(outer_glow, inner_glow), max(ember_core, max(ember_particle * 0.8, max(red_ember_particle * 0.9, ash_particles * 0.5))));
        burn_alpha = max(burn_alpha, blue_core * 0.4);
        burn_alpha = max(burn_alpha, length(spark_total) * 0.8);
        burn_alpha *= (0.9 + 0.2 * sin(t * 20.0 + edge_pos * 50.0 + 3.0) * sin(t * 17.0));
        burn_alpha *= progress_fade * edge_fade;
        
        burn_line_total += burn_line * burn_alpha;
        burn_alpha_total = max(burn_alpha_total, burn_alpha);
    }

    // === LEFT EDGE ===
    if ((burn_side == 2 || burn_side == 4 || burn_side == 6) && uvpos.y >= effective_bottom && uvpos.y <= effective_top) {
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
        
        float fiber_edge = (fiber - 0.5) * 0.008* wave_fade;;
        float wave = sin(edge_pos * 40.0 + t * 2.0 + 10.0) * 0.003 + sin(edge_pos * 80.0 - t * 3.0 + 14.0) * 0.002* wave_fade;;
        float edge_noise = smooth_hash(edge_pos * height * 0.5, t * 2.0 + 2.0) * 0.01* wave_fade;;
        float adjusted_dist = edge_dist + wave + edge_noise + fiber_edge;
        
        float abs_adj = abs(adjusted_dist);
        float turb_mod = 0.7 + 0.3 * flame_turb;
        
        float ember_core = smoothstep(0.012 * burn_size, 0.0, abs_adj) * smoothstep(-0.005 * burn_size, 0.005 * burn_size, adjusted_dist) * turb_mod;
        float inner_glow = smoothstep(0.025 * burn_size, 0.0, abs_adj) * smoothstep(-0.01 * burn_size, 0.01 * burn_size, adjusted_dist) * (0.85 + 0.15 * flame_turb);
        float outer_glow = smoothstep(0.05 * burn_size, 0.0, abs_adj) * smoothstep(-0.02 * burn_size, 0.02 * burn_size, adjusted_dist) * (0.9 + 0.1 * flame_turb);
        float edge_proximity = smoothstep(0.08, 0.0, abs_adj);
        
        // Sparks
        vec3 spark_total = vec3(0.0);
        for (float i = 0.0; i < 5.0; i += 1.0) {
            float spawn_y = smooth_hash1(i + floor(t * 0.3) * 13.0 + 100.0) * (effective_top - effective_bottom) + effective_bottom;
            spark_total += calc_spark_particle(uvpos, effective_left, spawn_y, t, i + 300.0, inv_aspect);
        }
        
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
        
        // Temperature-based colors
        float core_temp = 0.95 - abs_adj * 8.0;
        float inner_temp = 0.75 - abs_adj * 5.0;
        float outer_temp = 0.5 - abs_adj * 3.0;
        
        vec3 core_color = temperature_to_color(clamp(core_temp, 0.0, 1.0));
        vec3 inner_color = temperature_to_color(clamp(inner_temp, 0.0, 1.0));
        vec3 outer_color = temperature_to_color(clamp(outer_temp, 0.0, 1.0));
        
        vec3 base_ember = apply_flame_hue(vec3(1.0, 0.5, 0.0));
        vec3 base_red_ember = apply_flame_hue(vec3(1.0, 0.1, 0.0));
        vec3 ash_color = vec3(0.15, 0.12, 0.10);
        vec3 blue_flame = apply_flame_hue(vec3(0.3, 0.5, 1.0));
        
        float blue_core = smoothstep(0.006 * burn_size, 0.0, abs_adj) * 
                          smoothstep(-0.002 * burn_size, 0.002 * burn_size, adjusted_dist);
        
        vec3 hot_outer = velocity_heat_color(outer_color, burn_velocity, edge_proximity);
        vec3 hot_inner = velocity_heat_color(inner_color, burn_velocity, edge_proximity);
        vec3 hot_core = velocity_heat_color(core_color, burn_velocity * 1.5, edge_proximity);
        
        vec3 burn_line = hot_outer * outer_glow 
                       + hot_inner * inner_glow 
                       + hot_core * ember_core 
                       + blue_flame * blue_core * 0.35
                       + base_ember * ember_particle
                       + base_red_ember * red_ember_particle
                       + ash_color * ash_particles
                       + spark_total;
        
        float char_zone = smoothstep(0.005, -0.03, edge_dist);
        float char_texture = fiber * 0.4 + 0.6;
        float cracks = crack_pattern(uvpos * 12.0 + vec2(0.0, t * 0.05), edge_pos * 10.0 + 100.0);
        vec3 char_color = vec3(0.06, 0.03, 0.02) * char_texture;
        char_color = mix(char_color, vec3(0.02, 0.01, 0.005), cracks * 0.6);
        burn_line = mix(burn_line, char_color, char_zone * 0.55);
        
        float burn_alpha = max(max(outer_glow, inner_glow), max(ember_core, max(ember_particle * 0.8, max(red_ember_particle * 0.9, ash_particles * 0.5))));
        burn_alpha = max(burn_alpha, blue_core * 0.4);
        burn_alpha = max(burn_alpha, length(spark_total) * 0.8);
        burn_alpha *= (0.9 + 0.2 * sin(t * 20.0 + edge_pos * 50.0 + 6.0) * sin(t * 17.0));
        burn_alpha *= progress_fade * edge_fade;
        
        burn_line_total += burn_line * burn_alpha;
        burn_alpha_total = max(burn_alpha_total, burn_alpha);
    }

    // === RIGHT EDGE ===
    if ((burn_side == 3 || burn_side == 4 || burn_side == 6) && uvpos.y >= effective_bottom && uvpos.y <= effective_top) {
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
        
        float fiber_edge = (fiber - 0.5) * 0.008* wave_fade;;
        float wave = sin(edge_pos * 40.0 + t * 2.0 + 15.0) * 0.003 + sin(edge_pos * 80.0 - t * 3.0 + 21.0) * 0.002* wave_fade;;
        float edge_noise = smooth_hash(edge_pos * height * 0.5, t * 2.0 + 3.0) * 0.01* wave_fade;;
        float adjusted_dist = edge_dist + wave + edge_noise + fiber_edge;
        
        float abs_adj = abs(adjusted_dist);
        float turb_mod = 0.7 + 0.3 * flame_turb;
        
        float ember_core = smoothstep(0.012 * burn_size, 0.0, abs_adj) * smoothstep(-0.005 * burn_size, 0.005 * burn_size, adjusted_dist) * turb_mod;
        float inner_glow = smoothstep(0.025 * burn_size, 0.0, abs_adj) * smoothstep(-0.01 * burn_size, 0.01 * burn_size, adjusted_dist) * (0.85 + 0.15 * flame_turb);
        float outer_glow = smoothstep(0.05 * burn_size, 0.0, abs_adj) * smoothstep(-0.02 * burn_size, 0.02 * burn_size, adjusted_dist) * (0.9 + 0.1 * flame_turb);
        float edge_proximity = smoothstep(0.08, 0.0, abs_adj);
        
        // Sparks
        vec3 spark_total = vec3(0.0);
        for (float i = 0.0; i < 5.0; i += 1.0) {
            float spawn_y = smooth_hash1(i + floor(t * 0.3) * 17.0 + 150.0) * (effective_top - effective_bottom) + effective_bottom;
            spark_total += calc_spark_particle(uvpos, effective_right, spawn_y, t, i + 400.0, inv_aspect);
        }
        
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
        
        // Temperature-based colors
        float core_temp = 0.95 - abs_adj * 8.0;
        float inner_temp = 0.75 - abs_adj * 5.0;
        float outer_temp = 0.5 - abs_adj * 3.0;
        
        vec3 core_color = temperature_to_color(clamp(core_temp, 0.0, 1.0));
        vec3 inner_color = temperature_to_color(clamp(inner_temp, 0.0, 1.0));
        vec3 outer_color = temperature_to_color(clamp(outer_temp, 0.0, 1.0));
        
        vec3 base_ember = apply_flame_hue(vec3(1.0, 0.5, 0.0));
        vec3 base_red_ember = apply_flame_hue(vec3(1.0, 0.1, 0.0));
        vec3 ash_color = vec3(0.15, 0.12, 0.10);
        vec3 blue_flame = apply_flame_hue(vec3(0.3, 0.5, 1.0));
        
        float blue_core = smoothstep(0.006 * burn_size, 0.0, abs_adj) * 
                          smoothstep(-0.002 * burn_size, 0.002 * burn_size, adjusted_dist);
        
        vec3 hot_outer = velocity_heat_color(outer_color, burn_velocity, edge_proximity);
        vec3 hot_inner = velocity_heat_color(inner_color, burn_velocity, edge_proximity);
        vec3 hot_core = velocity_heat_color(core_color, burn_velocity * 1.5, edge_proximity);
        
        vec3 burn_line = hot_outer * outer_glow 
                       + hot_inner * inner_glow 
                       + hot_core * ember_core 
                       + blue_flame * blue_core * 0.35
                       + base_ember * ember_particle
                       + base_red_ember * red_ember_particle
                       + ash_color * ash_particles
                       + spark_total;
        
        float char_zone = smoothstep(0.005, -0.03, edge_dist);
        float char_texture = fiber * 0.4 + 0.6;
        float cracks = crack_pattern(uvpos * 12.0 + vec2(0.0, t * 0.05), edge_pos * 10.0 + 150.0);
        vec3 char_color = vec3(0.06, 0.03, 0.02) * char_texture;
        char_color = mix(char_color, vec3(0.02, 0.01, 0.005), cracks * 0.6);
        burn_line = mix(burn_line, char_color, char_zone * 0.55);
        
        float burn_alpha = max(max(outer_glow, inner_glow), max(ember_core, max(ember_particle * 0.8, max(red_ember_particle * 0.9, ash_particles * 0.5))));
        burn_alpha = max(burn_alpha, blue_core * 0.4);
        burn_alpha = max(burn_alpha, length(spark_total) * 0.8);
        burn_alpha *= (0.9 + 0.2 * sin(t * 20.0 + edge_pos * 50.0 + 9.0) * sin(t * 17.0));
        burn_alpha *= progress_fade * edge_fade;
        
        burn_line_total += burn_line * burn_alpha;
        burn_alpha_total = max(burn_alpha_total, burn_alpha);
    }
    
    result.rgb += burn_line_total * (1.0 - end_blur * 0.7);
    
    // === IMPROVED SMOKE WISPS with diffusion ===
    // From lecture: fire creates smoke that rises and diffuses
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
  
/*
    // === IMPROVED FLAME OVERLAY ===
{
    float flame_turbulence = 10.0;
    float flame_intensity = 1.5;
    float flame_rise_speed = 1.5;
    
    float distortion_scale = 0.5;
    vec2 distorted_uvpos = uvpos ;
    
    float flame_count = 30.0;
    float flame_scale = 0.08;
    
    float flame_y = (distorted_uvpos.y - progress) / flame_scale;
    
    vec3 total_flame_col = vec3(0.0);
    float total_flame_a = 0.0;
    
    // 4 layers for more depth
    for (int layer = 0; layer < 4; layer++) {
        float layer_f = float(layer);
        
        // X-axis offset - each layer shifted differently
        float layer_x_shift = (layer_f - 1.5) * 0.15;
        
        float tiled_x = fract((distorted_uvpos.x + layer_x_shift) * flame_count) - 0.5;
        
        if (flame_y > -10.0 && flame_y < 20.0) {
            
            float cell_id = floor((distorted_uvpos.x + layer_x_shift) * flame_count) + layer_f * 100.0;
            float cell_offset = hash1(cell_id * 7.3) * 0.8;
            float cell_height = 0.7 + hash1(cell_id * 13.7) * 1.4;
            
            // Back layers slightly taller
            cell_height *= 1.0 + (3.0 - layer_f) * 0.15;
            
            vec2 flame_uv = vec2(tiled_x * 0.6, flame_y / cell_height);
            
            float width_at_height = 1.0 - flame_uv.y * 0.5;
            vec2 p = vec2(flame_uv.x / max(width_at_height, 0.1), flame_uv.y);
            
            float T = burn_progress * flame_speed * flame_rise_speed + cell_offset + layer_f * 0.4;
            
            vec2 noisePos = vec2(p.x * 2.0 + cell_id, p.y * 3.0 - T);
            float n = advectedFBM_flame(noisePos, T, flame_rise_speed, flame_turbulence);
            n += noise3D_flame(vec3(p * 8.0, T * 2.0)) * 0.15;
            
            // Soften horizontal cell edges to prevent clipping
// Soften horizontal cell edges
float cell_edge_fade = 1.0 - pow(abs(tiled_x) * 2.0, 4.0);
cell_edge_fade = clamp(cell_edge_fade, 0.0, 1.0);

// Soften vertical edges - fade at top and bottom
float vert_fade = smoothstep(-0.5, 0.0, flame_uv.y) * (1.0 - smoothstep(0.7, 1.2, flame_uv.y));

float dist = length(vec2(p.x * 0.5, (p.y - 0.3) * 0.4));
float threshold = 0.3 + n * 0.25;
float density = smoothstep(threshold + 0.15, threshold - 0.15, dist);
density *= cell_edge_fade * vert_fade;
            
            // Stretch h to reach red - back layers reach higher h values
            float h_stretch = 1.8 + (3.0 - layer_f) * 0.3;
            float h = clamp(flame_uv.y * h_stretch, 0.0, 1.0);
            
            // Colors
            vec3 white_core = vec3(0.85, 0.1, 0.0);
            vec3 bright_yellow = vec3(1.0, 0.92, 0.3);
            vec3 yellow = vec3(1.0, 0.82, 0.1);
            vec3 gold = vec3(1.0, 0.7, 0.05);
            vec3 orange = vec3(1.0, 0.5, 0.02);
            vec3 red_orange = vec3(0.95, 0.3, 0.0);
            vec3 red = vec3(0.85, 0.1, 0.0);
            vec3 dark_red = vec3(0.5, 0.05, 0.0);
            
            vec3 flame_col;
            if (h < 0.12) {
                flame_col = mix(white_core, bright_yellow, h / 0.12);
            } else if (h < 0.25) {
                flame_col = mix(bright_yellow, yellow, (h - 0.12) / 0.13);
            } else if (h < 0.4) {
                flame_col = mix(yellow, gold, (h - 0.25) / 0.15);
            } else if (h < 0.55) {
                flame_col = mix(gold, orange, (h - 0.4) / 0.15);
            } else if (h < 0.7) {
                flame_col = mix(orange, red_orange, (h - 0.55) / 0.15);
            } else if (h < 0.85) {
                flame_col = mix(red_orange, red, (h - 0.7) / 0.15);
            } else {
                flame_col = mix(red, dark_red, (h - 0.85) / 0.15);
            }
            
            // Noise variation
            float color_noise = noise3D_flame(vec3(p * 4.0, T * 3.0)) * 0.1;
            flame_col = mix(flame_col, yellow, color_noise);
            
            // Layer brightness - front layers brighter
            float layer_bright = 0.7 + layer_f * 0.1;
            flame_col *= layer_bright;
            
            flame_col *= 1.0 + density * 0.8;
            
     //       float flicker = 0.88 + 0.12 * noise3D_flame(vec3(p * 6.0, T * 5.0));
    //        flame_col *= flicker;
            vec3(0.85, 0.1, 0.0);
            // Apply flame_color tint gently
            vec3 fc = flame_color.rgb;
            float saturation = max(fc.r, max(fc.g, fc.b)) - min(fc.r, min(fc.g, fc.b));
            if (saturation > 0.1) {
                vec3 tinted = flame_col * (fc * 0.5 + 0.5);
                flame_col = mix(flame_col, tinted, 0.25);
            }
            
            float simple_a = density;
            simple_a *= vert_fade;
            
            // Back layers slightly more transparent
       //     simple_a *= 0.6 + layer_f * 0.13;
            
            simple_a *= clamp(progress * 100.0, 0.0, 1.0);
            simple_a *= clamp(distorted_uvpos.x * (width / 30.0), 0.0, 1.0);
            simple_a *= clamp((1.0 - distorted_uvpos.x) * (width / 30.0), 0.0, 1.0);
            
            float glow = smoothstep(0.4, 0.0, dist) * 0.3;
            simple_a = max(simple_a, glow * (1.0 - h));
            
            total_flame_col = total_flame_col * (1.0 - simple_a) + flame_col * simple_a;
            total_flame_a = max(total_flame_a, simple_a);
        }
    }
    
    result.rgb = mix(result.rgb, total_flame_col, total_flame_a);
    result.a = max(result.a, total_flame_a);
}


{
    float flame_turbulence = 100.5;
    float flame_intensity = 3.0;
    float flame_rise_speed = 100.2;
    
    // Distortion
    float distortion_scale = 8.0;
    vec2 distorted_uvpos = uvpos + (vec2(
        smooth_noise2D(uvpos * distortion_scale + t / distortion_scale),
        smooth_noise2D(uvpos * distortion_scale - t / distortion_scale)) *
        0.1 * (1.0 - burn_progress) - (1.0 - burn_progress) * 0.05);
    
    // === KEY FIX: Tile flames along the burn edge ===
    float flame_count = 50.0; // Number of flames across width
    float flame_scale = 0.3; // Height of each flame in UV space
    
    // Create tiled X coordinate (repeating flames)
    float tiled_x = fract(distorted_uvpos.x * flame_count) - 0.5;
    
    // Y coordinate relative to burn edge
    float flame_y = (distorted_uvpos.y - progress) / flame_scale;
    
    // Only render flames near the burn edge and above it
    if (flame_y > -0.5 && flame_y < 1.5) {
        
        // Add per-flame variation using cell index
        float cell_id = floor(distorted_uvpos.x * flame_count);
        float cell_offset = hash1(cell_id * 7.3) * 0.5; // Random phase per flame
        float cell_height = 0.7 + hash1(cell_id * 13.7) * 0.6; // Random height
        float cell_width = 0.5 + hash1(cell_id * 23.1) * 10.9; 
        
        // Transform to flame-local coordinates
        vec2 flame_uv = vec2(tiled_x * 0.6, flame_y / cell_height);  // 0.3 = thick, try 0.1 for even thicker
        
        // Flame width tapers with height
        float width_at_height = 1.0 - flame_uv.y * 0.4;
        vec2 p = vec2(flame_uv.x / max(width_at_height, 0.1), flame_uv.y);
        
        float T = burn_progress * flame_speed * flame_rise_speed + cell_offset;
        
        // Noise for flame shape
        vec2 noisePos = vec2(p.x * 2.0 + cell_id, p.y * 3.0 - T);
        float n = advectedFBM_flame(noisePos, T, flame_rise_speed, flame_turbulence);
        float detail = noise3D_flame(vec3(p * 8.0, T * 2.0)) * 0.15;
        n += detail;
        
        // Density calculation
        float dist = length(vec2(p.x * 0.8, (p.y - 0.3) * 0.6));
        float threshold = 0.25 + n * 0.2;
        float density = smoothstep(threshold + 0.08, threshold - 0.08, dist);
        
        // Layered density for translucency
        float layered_density = 0.0;
        for(int i = 0; i < 3; i++) {
            float offset = float(i) * 0.1;
            float layer_n = advectedFBM_flame(noisePos + vec2(offset), T + offset, flame_rise_speed, flame_turbulence);
            float layer_dist = length(vec2(p.x * 0.8, (p.y - 0.3) * 0.6));  
            float layer_thresh = 0.25 + layer_n * 0.2;
            layered_density += smoothstep(layer_thresh + 0.08, layer_thresh - 0.08, layer_dist) * (1.0 - float(i) * 0.25);
        }
        layered_density /= 2.0;
        
        // Temperature field
        float height_cooling = 1.0 - pow(clamp(flame_uv.y, 0.0, 1.0), 0.6);
        float density_heat = layered_density * 0.5 + 0.5;
        float flicker = noise3D_flame(vec3(p * 8.0 + cell_id, T * 4.0)) * 0.2 + 0.8;
        float temperature = clamp(height_cooling * density_heat * flicker, 0.0, 1.0);
        
        // Color from temperature
        vec3 simple_flame_col = temperatureToFlameColor(temperature, layered_density, flame_intensity);
        simple_flame_col *= normalize(flame_color.rgb + vec3(0.1)) * 1.5;
        
        // Alpha calculation
        float simple_a = layered_density;
        simple_a *= 1.0 - smoothstep(0.6, 1.0, flame_uv.y); // Fade at top
        simple_a *= smoothstep(-0.2, 0.1, flame_uv.y); // Fade at bottom
        simple_a = pow(clamp(simple_a, 0.0, 1.0), 0.8);
        
        // Edge fades
        simple_a *= clamp(progress * 100.0, 0.0, 1.0);
        simple_a *= clamp(distorted_uvpos.x * (width / 30.0), 0.0, 1.0);
        simple_a *= clamp((1.0 - distorted_uvpos.x) * (width / 30.0), 0.0, 1.0);
        
        // Glow
        float glow = density * 0.1;
        vec3 glow_color = vec3(1.0, 0.3, 0.0) * glow * height_cooling;
        simple_flame_col = mix(glow_color, simple_flame_col, smoothstep(0.0, 0.3, simple_a));
        simple_a = max(simple_a, glow * 0.5);
        
        // Blend
        result.rgb = mix(result.rgb, simple_flame_col, simple_a);
        result.a = max(result.a, simple_a);
    }
}

{
    float flame_turbulence = 2.5;
    float flame_intensity = 1.8;
    float flame_rise_speed = 1.5;
    
    float distortion_scale = 12.0;
    vec2 distorted_uvpos = uvpos + (vec2(
        smooth_noise2D(uvpos * distortion_scale + t / distortion_scale),
        smooth_noise2D(uvpos * distortion_scale - t / distortion_scale)) *
        0.03 * (1.0 - burn_progress) - (1.0 - burn_progress) * 0.015);
    
    float flame_count = 45.0;
    float flame_scale = 0.12;
    
    float flame_y = (distorted_uvpos.y - progress) / flame_scale;
    
    vec3 total_flame_col = vec3(0.0);
    float total_flame_a = 0.0;
    
    // 5 layers for realistic depth
    for (int layer = 0; layer < 5; layer++) {
        float layer_f = float(layer);
        
        // Each layer offset on X axis
        float layer_x_shift = (layer_f - 2.0) * 0.12;
        
        float tiled_x = fract((distorted_uvpos.x + layer_x_shift) * flame_count) - 0.5;
        
        if (flame_y > -1.0 && flame_y < 2.5) {
            
            float cell_id = floor((distorted_uvpos.x + layer_x_shift) * flame_count) + layer_f * 100.0;
            float cell_offset = hash1(cell_id * 7.3) * 1.2;
            float cell_height = 0.6 + hash1(cell_id * 13.7) * 0.8;
            
            // Back layers taller
            cell_height *= 1.0 + (4.0 - layer_f) * 0.12;
            
            // Per-flame width variation
            float cell_width = 0.4 + hash1(cell_id * 31.7) * 0.4;
            
            vec2 flame_uv = vec2(tiled_x * cell_width, flame_y / cell_height);
            
            float width_at_height = 1.0 - flame_uv.y * 0.55;
            vec2 p = vec2(flame_uv.x / max(width_at_height, 0.05), flame_uv.y);
            
            float T = burn_progress * flame_speed * flame_rise_speed + cell_offset + layer_f * 0.3;
            
            vec2 noisePos = vec2(p.x * 1.5 + cell_id * 0.1, p.y * 2.5 - T);
            float n = advectedFBM_flame(noisePos, T, flame_rise_speed, flame_turbulence);
            n += noise3D_flame(vec3(p * 6.0, T * 1.5)) * 0.2;
            
            // Soft cell edges
            float cell_edge_fade = 1.0 - pow(abs(tiled_x) * 1.8, 3.0);
            cell_edge_fade = clamp(cell_edge_fade, 0.0, 1.0);
            
            // Soft vertical fade
            float vert_fade = smoothstep(-0.3, 0.05, flame_uv.y) * (1.0 - smoothstep(0.6, 1.1, flame_uv.y));
            
            float dist = length(vec2(p.x * 0.4, (p.y - 0.25) * 0.35));
            float threshold = 0.35 + n * 0.2;
            float density = smoothstep(threshold + 0.2, threshold - 0.1, dist);
            density *= cell_edge_fade * vert_fade;
            
            // Height for color
            float h_stretch = 1.5 + (4.0 - layer_f) * 0.2;
            float h = clamp(flame_uv.y * h_stretch, 0.0, 1.0);
            
            // Realistic fire colors
            vec3 inner_core = vec3(1.0, 0.97, 0.85);
            vec3 bright_yellow = vec3(1.0, 0.9, 0.35);
            vec3 yellow = vec3(1.0, 0.75, 0.12);
            vec3 gold = vec3(1.0, 0.6, 0.05);
            vec3 orange = vec3(0.98, 0.45, 0.02);
            vec3 deep_orange = vec3(0.92, 0.32, 0.01);
            vec3 red_orange = vec3(0.85, 0.2, 0.0);
            vec3 red = vec3(0.7, 0.1, 0.0);
            vec3 dark_red = vec3(0.4, 0.03, 0.0);
            
            vec3 flame_col;
            if (h < 0.08) {
                flame_col = mix(inner_core, bright_yellow, h / 0.08);
            } else if (h < 0.18) {
                flame_col = mix(bright_yellow, yellow, (h - 0.08) / 0.1);
            } else if (h < 0.3) {
                flame_col = mix(yellow, gold, (h - 0.18) / 0.12);
            } else if (h < 0.42) {
                flame_col = mix(gold, orange, (h - 0.3) / 0.12);
            } else if (h < 0.55) {
                flame_col = mix(orange, deep_orange, (h - 0.42) / 0.13);
            } else if (h < 0.68) {
                flame_col = mix(deep_orange, red_orange, (h - 0.55) / 0.13);
            } else if (h < 0.82) {
                flame_col = mix(red_orange, red, (h - 0.68) / 0.14);
            } else {
                flame_col = mix(red, dark_red, (h - 0.82) / 0.18);
            }
            
            // Subtle color noise
            float color_noise = noise3D_flame(vec3(p * 3.0, T * 2.0)) * 0.08;
            flame_col = mix(flame_col, yellow, color_noise);
            
            // Layer brightness - front brighter
            float layer_bright = 0.65 + layer_f * 0.09;
            flame_col *= layer_bright;
            
            // Density brightness
            flame_col *= 1.0 + density * 0.5;
            
            // Realistic flicker
            float flicker = 0.92 + 0.08 * noise3D_flame(vec3(p * 5.0, T * 4.0));
            flicker *= 0.95 + 0.05 * noise3D_flame(vec3(p * 12.0, T * 8.0));
            flame_col *= flicker;
            
            // Gentle tint from flame_color
            vec3 fc = flame_color.rgb;
            float saturation = max(fc.r, max(fc.g, fc.b)) - min(fc.r, min(fc.g, fc.b));
            if (saturation > 0.1) {
                vec3 tinted = flame_col * (fc * 0.4 + 0.6);
                flame_col = mix(flame_col, tinted, 0.2);
            }
            
            float simple_a = density;
            simple_a *= vert_fade;
            
            // Back layers more transparent
            simple_a *= 0.5 + layer_f * 0.12;
            
            simple_a *= clamp(progress * 100.0, 0.0, 1.0);
            simple_a *= clamp(distorted_uvpos.x * (width / 30.0), 0.0, 1.0);
            simple_a *= clamp((1.0 - distorted_uvpos.x) * (width / 30.0), 0.0, 1.0);
            
            // Soft glow
            float glow = smoothstep(0.5, 0.0, dist) * 0.2 * (1.0 - h * 0.5);
            simple_a = max(simple_a, glow);
            
            // Blend layers back to front
            total_flame_col = total_flame_col * (1.0 - simple_a) + flame_col * simple_a;
            total_flame_a = max(total_flame_a, simple_a * 0.95);
        }
    }
    
    result.rgb = mix(result.rgb, total_flame_col, total_flame_a);
    result.a = max(result.a, total_flame_a);
}


*/



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
