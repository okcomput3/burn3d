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

// ============ 3D FIRE FUNCTIONS ============
vec4 tanh_approx(vec4 x)
{
    vec4 x2 = x * x;
    return clamp(x * (27.0 + x2) / (27.0 + 9.0 * x2), -1.0, 1.0);
}

vec4 render_fire(vec2 I, vec3 iResolution, float t)
{
    float z = 0.0;
    float d = 0.0;
    vec4 O = vec4(0.0);
    
    // Pre-compute values used in loop
    float t_div_01 = t * 10.0; // t / 0.1
    float cos_t = cos(t);
    
    for (int iter = 0; iter < 32; iter++)
    {
        vec3 p = z * normalize(vec3(I + I, 0.0) - iResolution.xyy);
        p.z += 5.0 + cos_t;
        
        float angle = p.y * 0.5;
        // Pre-compute cos values
        float c0 = cos(angle);
        float c1 = cos(angle + 33.0);
        float c2 = cos(angle + 11.0);
        mat2 m = mat2(c0, c1, c2, c0);
        
        float inv_scale = 1.0 / max(p.y * 0.1 + 1.0, 0.1);
        p.xz = m * p.xz * inv_scale;
        
        // Unroll the inner loop - same values as original: d starts at 2, divides by 0.6 each time
        // d = 2.0, 3.333, 5.555, 9.259, 15.432 (stops when >= 15)
        // So we have 4 iterations: 2.0, 3.333, 5.555, 9.259
        vec3 time_vec = vec3(t_div_01, t, 0.0);
        
        time_vec.z = 2.0;
        p += cos((p.yzx - time_vec) * 2.0) * 0.5;
        
        time_vec.z = 3.333333;
        p += cos((p.yzx - time_vec) * 3.333333) * 0.3;
        
        time_vec.z = 5.555556;
        p += cos((p.yzx - time_vec) * 5.555556) * 0.18;
        
        time_vec.z = 9.259259;
        p += cos((p.yzx - time_vec) * 9.259259) * 0.108;
        
        d = 0.01 + abs(length(p.xz) + p.y * 0.3 - 0.5) * 0.142857; // 1/7 = 0.142857
        z += d;
        
        if (d < 0.80) O += (sin(z * 0.333333 + vec4(7.0, 2.0, 3.0, 0.0)) + 1.1) / d;
    }
    return tanh_approx(O * 0.0005); // 1/2000 = 0.0005
}

// Simple hash for burn line noise
float hash1(float p) { return fract(sin(p * 127.1) * 43758.5453123); }
float hash2(vec2 p) { return fract(sin(dot(p, vec2(127.1, 311.7))) * 43758.5453123); }

// Calculate wave offset for a given position along an edge
float calc_wave_offset(float pos, float t, float wave_fade) {
    float wave_offset = 0.0;
    wave_offset += sin(pos * 12.0 + t * 0.4) * 0.025;
    wave_offset += sin(pos * 25.0 - t * 0.7) * 0.015;
    wave_offset += sin(pos * 45.0 + t * 1.2) * 0.008;
    wave_offset += sin(pos * 8.0 + t * 0.2) * 0.035;
    wave_offset += (hash2(vec2(floor(pos * 15.0), floor(t * 0.3))) - 0.5) * 0.02;
    return wave_offset * wave_fade;
}

void main()
{
    float width = size.x;
    float height = size.y;
    
    float burn_progress = progress;
    if (direction == 1) burn_progress = 1.0 - burn_progress;
    
    float t = burn_progress * flame_speed * 10.0;
    
    // ============ WAVY BURN LINE - ALL SIDES ============
    float wave_fade = smoothstep(0.0, 0.1, progress) * smoothstep(1.0, 0.85, progress);
    
    // Calculate effective progress from each side (0 to 0.5 range, meeting at center)
    float side_progress = progress * 0.5;
    
    // Bottom edge (burning upward)
    float wave_bottom = calc_wave_offset(uvpos.x, t, wave_fade);
    float effective_bottom = side_progress + wave_bottom;
    float dist_from_bottom = uvpos.y - effective_bottom;
    
    // Top edge (burning downward)
    float wave_top = calc_wave_offset(uvpos.x, t + 5.0, wave_fade);
    float effective_top = 1.0 - side_progress - wave_top;
    float dist_from_top = effective_top - uvpos.y;
    
    // Left edge (burning rightward)
    float wave_left = calc_wave_offset(uvpos.y, t + 10.0, wave_fade);
    float effective_left = side_progress + wave_left;
    float dist_from_left = uvpos.x - effective_left;
    
    // Right edge (burning leftward)
    float wave_right = calc_wave_offset(uvpos.y, t + 15.0, wave_fade);
    float effective_right = 1.0 - side_progress - wave_right;
    float dist_from_right = effective_right - uvpos.x;
    
    // Check if pixel is inside the unburned area (inside all four boundaries)
    bool inside_unburned = (uvpos.y >= effective_bottom && uvpos.y <= effective_top &&
                            uvpos.x >= effective_left && uvpos.x <= effective_right);
    
    // Find the minimum distance (closest burn edge)
    float dist_from_burn = min(min(dist_from_bottom, dist_from_top), min(dist_from_left, dist_from_right));
    
    vec4 fire_color_accum = vec4(0.0);
    float a_3d = 0.0;

    // ============ 3D FIRE - FROM ALL SIDES ============
    // Only render fire near the boundary, and only on the unburned side
    if (dist_from_burn >= -0.05 && dist_from_burn <= 0.5)
    {
        float fire_scale = 0.2 + flame_height;
        float fire_width_scale = max(width * 0.12, 5.0) * (1.5 + flame_width);
        float fire_res = max(height * 0.15, 40.0);
        vec3 iResolution = vec3(fire_width_scale, fire_res, fire_res);
        float spacing = fire_width_scale * 0.5;
        
        // Pre-compute offsets and shifts arrays as constants
        // offsets: 0.0, 1.5, 3.0, 4.5, 4.5, 4.5
        // shifts: 0.0, 0.25, 0.5, 0.75, 0.65, 0.35
        
        // Pre-compute common bounds checks
        bool in_horizontal_bounds = uvpos.x >= effective_left && uvpos.x <= effective_right;
        bool in_vertical_bounds = uvpos.y >= effective_bottom && uvpos.y <= effective_top;
        
        // Pre-compute loop bounds
        float loop_start = -spacing * 2.0;
        float loop_end_w = width + spacing * 2.0;
        float loop_end_h = height + spacing * 2.0;

        // Fire from bottom edge
        if (dist_from_bottom >= -0.05 && dist_from_bottom <= 0.3 && in_horizontal_bounds) {
            float fireY = (dist_from_bottom - 0.05) * height * fire_scale;
            float uvpos_x_width = uvpos.x * width;
            
            for(int i = 0; i < 6; i++) {
                float offset_val = (i < 3) ? float(i) * 1.5 : 4.5;
                float shift_val;
                if (i == 0) shift_val = 0.0;
                else if (i == 1) shift_val = 0.25;
                else if (i == 2) shift_val = 0.5;
                else if (i == 3) shift_val = 0.75;
                else if (i == 4) shift_val = 0.65;
                else shift_val = 0.35;
                
                float width_mult = (i < 2) ? 1.0 : 1.5;
                float max_localX = fire_width_scale * width_mult;
                
                for (float off = loop_start + spacing * shift_val; off <= loop_end_w; off += spacing) {
                    float localX = uvpos_x_width - off - spacing * 0.5;
                    if (abs(localX) < max_localX) {
                        fire_color_accum += render_fire(vec2(localX, fireY), iResolution, t + off * 0.01 + offset_val) * 0.5;
                    }
                }
            }
        }
        
        // Fire from top edge
        if (dist_from_top >= -0.05 && dist_from_top <= 0.3 && in_horizontal_bounds) {
            float fireY = (dist_from_top - 0.05) * height * fire_scale;
            float uvpos_x_width = uvpos.x * width;
            
            for(int i = 0; i < 6; i++) {
                float offset_val = (i < 3) ? float(i) * 1.5 : 4.5;
                float shift_val;
                if (i == 0) shift_val = 0.0;
                else if (i == 1) shift_val = 0.25;
                else if (i == 2) shift_val = 0.5;
                else if (i == 3) shift_val = 0.75;
                else if (i == 4) shift_val = 0.65;
                else shift_val = 0.35;
                
                float width_mult = (i < 2) ? 1.0 : 1.5;
                float max_localX = fire_width_scale * width_mult;
                
                for (float off = loop_start + spacing * shift_val; off <= loop_end_w; off += spacing) {
                    float localX = uvpos_x_width - off - spacing * 0.5;
                    if (abs(localX) < max_localX) {
                        fire_color_accum += render_fire(vec2(localX, fireY), iResolution, t + off * 0.01 + offset_val + 20.0) * 0.5;
                    }
                }
            }
        }
        
        // Fire from left edge
        if (dist_from_left >= -0.05 && dist_from_left <= 0.3 && in_vertical_bounds) {
            float fireY = (dist_from_left - 0.05) * width * fire_scale;
            float uvpos_y_height = uvpos.y * height;
            
            for(int i = 0; i < 6; i++) {
                float offset_val = (i < 3) ? float(i) * 1.5 : 4.5;
                float shift_val;
                if (i == 0) shift_val = 0.0;
                else if (i == 1) shift_val = 0.25;
                else if (i == 2) shift_val = 0.5;
                else if (i == 3) shift_val = 0.75;
                else if (i == 4) shift_val = 0.65;
                else shift_val = 0.35;
                
                float width_mult = (i < 2) ? 1.0 : 1.5;
                float max_localX = fire_width_scale * width_mult;
                
                for (float off = loop_start + spacing * shift_val; off <= loop_end_h; off += spacing) {
                    float localX = uvpos_y_height - off - spacing * 0.5;
                    if (abs(localX) < max_localX) {
                        fire_color_accum += render_fire(vec2(localX, fireY), iResolution, t + off * 0.01 + offset_val + 40.0) * 0.5;
                    }
                }
            }
        }
        
        // Fire from right edge
        if (dist_from_right >= -0.05 && dist_from_right <= 0.3 && in_vertical_bounds) {
            float fireY = (dist_from_right - 0.05) * width * fire_scale;
            float uvpos_y_height = uvpos.y * height;
            
            for(int i = 0; i < 6; i++) {
                float offset_val = (i < 3) ? float(i) * 1.5 : 4.5;
                float shift_val;
                if (i == 0) shift_val = 0.0;
                else if (i == 1) shift_val = 0.25;
                else if (i == 2) shift_val = 0.5;
                else if (i == 3) shift_val = 0.75;
                else if (i == 4) shift_val = 0.65;
                else shift_val = 0.35;
                
                float width_mult = (i < 2) ? 1.0 : 1.5;
                float max_localX = fire_width_scale * width_mult;
                
                for (float off = loop_start + spacing * shift_val; off <= loop_end_h; off += spacing) {
                    float localX = uvpos_y_height - off - spacing * 0.5;
                    if (abs(localX) < max_localX) {
                        fire_color_accum += render_fire(vec2(localX, fireY), iResolution, t + off * 0.01 + offset_val + 60.0) * 0.5;
                    }
                }
            }
        }
        
        fire_color_accum = clamp(fire_color_accum, 0.0, 1.0);
        
        a_3d = (fire_color_accum.r + fire_color_accum.g + fire_color_accum.b) * 0.333333;
        a_3d = smoothstep(0.45, 0.9, a_3d);
        a_3d *= clamp(progress * 10.0, 0.0, 1.0);
    }
    
    // ============ BACKGROUND & DISTORTION ============
    float distort_amount = a_3d * 0.02;
    vec2 distort_uv = uvpos + vec2(
        sin(uvpos.y * 30.0 + t * 5.5) * distort_amount,
        cos(uvpos.x * 30.0 + t * 4.0) * distort_amount
    );
    distort_uv = clamp(distort_uv, 0.0, 1.0);
    
    vec4 bg = get_pixel(distort_uv);
    
    // Hide burned areas (outside the unburned rectangle)
    if (!inside_unburned) {
        bg = vec4(0.0);
    }
    

    // ====== Charred edge - only inside unburned area ======
    if (inside_unburned) {
        float char_zone = smoothstep(0.0, 0.03, dist_from_burn) * smoothstep(0.06, 0.03, dist_from_burn);
        vec3 char_color = vec3(0.1, 0.05, 0.0);
        
        float char_delay = 0.1;
        float char_fade_duration = 0.15;
        float char_progress_factor;
        if (direction == 1) {
            char_progress_factor = smoothstep(char_delay + char_fade_duration, char_delay, 1.0 - progress);
        } else {
            char_progress_factor = smoothstep(char_delay, char_delay + char_fade_duration, progress);
        }
        
        bg.rgb = mix(bg.rgb, char_color, char_zone * 0.7 * char_progress_factor);
    }

    // ====== Blue fire zone - only inside unburned area ======
    if (inside_unburned) {
        float blue_height = 0.13;
        float blue_start = 0.02;
        float blue_zone = smoothstep(blue_start, blue_start + 0.02, dist_from_burn) 
                        * smoothstep(blue_start + blue_height, blue_start + blue_height * 0.5, dist_from_burn);
        
        float blue_flicker = 0.7 + 0.3 * sin(t * 12.0 + uvpos.x * 30.0) * sin(t * 8.0 + uvpos.x * 50.0);
        float blue_noise = sin(uvpos.x * 60.0 + t * 3.0) * 0.3 + sin(uvpos.x * 120.0 - t * 5.0) * 0.2;
        
        blue_zone *= blue_flicker * (0.8 + blue_noise * 0.4);
        blue_zone = clamp(blue_zone, 0.0, 1.0);
        
        float blue_delay = 0.15;
        float blue_fade_duration = 0.2;
        float blue_progress_factor;
        if (direction == 1) {
            blue_progress_factor = smoothstep(blue_delay + blue_fade_duration, blue_delay, 1.0 - progress);
        } else {
            blue_progress_factor = smoothstep(blue_delay, blue_delay + blue_fade_duration, progress);
        }
        blue_zone *= blue_progress_factor;
        
        vec3 blue_color = mix(vec3(0.0, 0.2, 0.8), vec3(0.3, 0.5, 1.0), smoothstep(blue_start, blue_start + blue_height * 0.7, dist_from_burn));
        bg.rgb = mix(bg.rgb, blue_color, blue_zone * 0.85);
    }

    // Composite 3D fire
    vec4 result = vec4(fire_color_accum.rgb, 1.0) * a_3d + bg * (1.0 - a_3d);
    
    // ============ BURN LINE - ALL SIDES ============
    float burn_size = 4.0;
    
    vec3 burn_line_total = vec3(0.0);
    float burn_alpha_total = 0.0;
    
    // Pre-compute common values for burn lines
    float progress_fade = clamp(progress * 10.0, 0.0, 1.0);
    float width_scale = width * 0.5;
    float height_scale = height * 0.5;
    float inv_aspect = width / height;
    
    // Bottom edge burn line
    if (uvpos.x >= effective_left && uvpos.x <= effective_right) {
        float edge_dist = dist_from_bottom;
        float edge_pos = uvpos.x;
        
        float wave = sin(edge_pos * 40.0 + t * 2.0) * 0.003 
                   + sin(edge_pos * 80.0 - t * 3.0) * 0.002;
        float edge_noise = hash2(vec2(floor(edge_pos * width_scale), floor(t * 2.0))) * 0.01;
        float adjusted_dist = edge_dist + wave + edge_noise;
        
        float abs_adj = abs(adjusted_dist);
        float ember_core = smoothstep(0.012 * burn_size, 0.0, abs_adj) 
                         * smoothstep(-0.005 * burn_size, 0.005 * burn_size, adjusted_dist);
        float inner_glow = smoothstep(0.025 * burn_size, 0.0, abs_adj) 
                         * smoothstep(-0.01 * burn_size, 0.01 * burn_size, adjusted_dist);
        float outer_glow = smoothstep(0.05 * burn_size, 0.0, abs_adj) 
                         * smoothstep(-0.02 * burn_size, 0.02 * burn_size, adjusted_dist);
        
        float ember_particle = 0.0;
        float horiz_range = effective_right - effective_left;
        for (float i = 0.0; i < 3.0; i += 1.0) {
            float px = hash1(i + floor(t * 0.5)) * horiz_range + effective_left;
            float py = effective_bottom + sin(t * (2.0 + i) + i * 3.14159) * 0.015 * burn_size;
            float spark_dist = length(vec2((uvpos.x - px) * inv_aspect, uvpos.y - py));
            ember_particle += smoothstep(0.02 * burn_size, 0.0, spark_dist) * (0.5 + 0.5 * sin(t * 10.0 + i * 5.0));
        }
        
        vec3 burn_line = vec3(0.8, 0.2, 0.0) * outer_glow 
                       + vec3(1.0, 0.6, 0.1) * inner_glow 
                       + vec3(1.0, 0.95, 0.7) * ember_core 
                       + vec3(1.0, 0.5, 0.0) * ember_particle;
        float burn_alpha = max(max(outer_glow, inner_glow), max(ember_core, ember_particle * 0.8));
        burn_alpha *= (0.9 + 0.2 * sin(t * 20.0 + edge_pos * 50.0) * sin(t * 17.0));
        burn_alpha *= progress_fade;
        
        burn_line_total += burn_line * burn_alpha;
        burn_alpha_total = max(burn_alpha_total, burn_alpha);
    }
    
    // Top edge burn line
    if (uvpos.x >= effective_left && uvpos.x <= effective_right) {
        float edge_dist = dist_from_top;
        float edge_pos = uvpos.x;
        
        float wave = sin(edge_pos * 40.0 + t * 2.0 + 5.0) * 0.003 
                   + sin(edge_pos * 80.0 - t * 3.0 + 7.0) * 0.002;
        float edge_noise = hash2(vec2(floor(edge_pos * width_scale), floor(t * 2.0) + 1.0)) * 0.01;
        float adjusted_dist = edge_dist + wave + edge_noise;
        
        float abs_adj = abs(adjusted_dist);
        float ember_core = smoothstep(0.012 * burn_size, 0.0, abs_adj) 
                         * smoothstep(-0.005 * burn_size, 0.005 * burn_size, adjusted_dist);
        float inner_glow = smoothstep(0.025 * burn_size, 0.0, abs_adj) 
                         * smoothstep(-0.01 * burn_size, 0.01 * burn_size, adjusted_dist);
        float outer_glow = smoothstep(0.05 * burn_size, 0.0, abs_adj) 
                         * smoothstep(-0.02 * burn_size, 0.02 * burn_size, adjusted_dist);
        
        float ember_particle = 0.0;
        float horiz_range = effective_right - effective_left;
        for (float i = 0.0; i < 3.0; i += 1.0) {
            float px = hash1(i + floor(t * 0.5) + 10.0) * horiz_range + effective_left;
            float py = effective_top + sin(t * (2.0 + i) + i * 3.14159 + 2.0) * 0.015 * burn_size;
            float spark_dist = length(vec2((uvpos.x - px) * inv_aspect, uvpos.y - py));
            ember_particle += smoothstep(0.02 * burn_size, 0.0, spark_dist) * (0.5 + 0.5 * sin(t * 10.0 + i * 5.0 + 1.0));
        }
        
        vec3 burn_line = vec3(0.8, 0.2, 0.0) * outer_glow 
                       + vec3(1.0, 0.6, 0.1) * inner_glow 
                       + vec3(1.0, 0.95, 0.7) * ember_core 
                       + vec3(1.0, 0.5, 0.0) * ember_particle;
        float burn_alpha = max(max(outer_glow, inner_glow), max(ember_core, ember_particle * 0.8));
        burn_alpha *= (0.9 + 0.2 * sin(t * 20.0 + edge_pos * 50.0 + 3.0) * sin(t * 17.0));
        burn_alpha *= progress_fade;
        
        burn_line_total += burn_line * burn_alpha;
        burn_alpha_total = max(burn_alpha_total, burn_alpha);
    }
    
    // Left edge burn line
    if (uvpos.y >= effective_bottom && uvpos.y <= effective_top) {
        float edge_dist = dist_from_left;
        float edge_pos = uvpos.y;
        
        float wave = sin(edge_pos * 40.0 + t * 2.0 + 10.0) * 0.003 
                   + sin(edge_pos * 80.0 - t * 3.0 + 14.0) * 0.002;
        float edge_noise = hash2(vec2(floor(edge_pos * height_scale), floor(t * 2.0) + 2.0)) * 0.01;
        float adjusted_dist = edge_dist + wave + edge_noise;
        
        float abs_adj = abs(adjusted_dist);
        float ember_core = smoothstep(0.012 * burn_size, 0.0, abs_adj) 
                         * smoothstep(-0.005 * burn_size, 0.005 * burn_size, adjusted_dist);
        float inner_glow = smoothstep(0.025 * burn_size, 0.0, abs_adj) 
                         * smoothstep(-0.01 * burn_size, 0.01 * burn_size, adjusted_dist);
        float outer_glow = smoothstep(0.05 * burn_size, 0.0, abs_adj) 
                         * smoothstep(-0.02 * burn_size, 0.02 * burn_size, adjusted_dist);
        
        float ember_particle = 0.0;
        float vert_range = effective_top - effective_bottom;
        for (float i = 0.0; i < 3.0; i += 1.0) {
            float py = hash1(i + floor(t * 0.5) + 20.0) * vert_range + effective_bottom;
            float px = effective_left + sin(t * (2.0 + i) + i * 3.14159 + 4.0) * 0.015 * burn_size;
            float spark_dist = length(vec2((uvpos.x - px) * inv_aspect, uvpos.y - py));
            ember_particle += smoothstep(0.02 * burn_size, 0.0, spark_dist) * (0.5 + 0.5 * sin(t * 10.0 + i * 5.0 + 2.0));
        }
        
        vec3 burn_line = vec3(0.8, 0.2, 0.0) * outer_glow 
                       + vec3(1.0, 0.6, 0.1) * inner_glow 
                       + vec3(1.0, 0.95, 0.7) * ember_core 
                       + vec3(1.0, 0.5, 0.0) * ember_particle;
        float burn_alpha = max(max(outer_glow, inner_glow), max(ember_core, ember_particle * 0.8));
        burn_alpha *= (0.9 + 0.2 * sin(t * 20.0 + edge_pos * 50.0 + 6.0) * sin(t * 17.0));
        burn_alpha *= progress_fade;
        
        burn_line_total += burn_line * burn_alpha;
        burn_alpha_total = max(burn_alpha_total, burn_alpha);
    }
    
    // Right edge burn line
    if (uvpos.y >= effective_bottom && uvpos.y <= effective_top) {
        float edge_dist = dist_from_right;
        float edge_pos = uvpos.y;
        
        float wave = sin(edge_pos * 40.0 + t * 2.0 + 15.0) * 0.003 
                   + sin(edge_pos * 80.0 - t * 3.0 + 21.0) * 0.002;
        float edge_noise = hash2(vec2(floor(edge_pos * height_scale), floor(t * 2.0) + 3.0)) * 0.01;
        float adjusted_dist = edge_dist + wave + edge_noise;
        
        float abs_adj = abs(adjusted_dist);
        float ember_core = smoothstep(0.012 * burn_size, 0.0, abs_adj) 
                         * smoothstep(-0.005 * burn_size, 0.005 * burn_size, adjusted_dist);
        float inner_glow = smoothstep(0.025 * burn_size, 0.0, abs_adj) 
                         * smoothstep(-0.01 * burn_size, 0.01 * burn_size, adjusted_dist);
        float outer_glow = smoothstep(0.05 * burn_size, 0.0, abs_adj) 
                         * smoothstep(-0.02 * burn_size, 0.02 * burn_size, adjusted_dist);
        
        float ember_particle = 0.0;
        float vert_range = effective_top - effective_bottom;
        for (float i = 0.0; i < 3.0; i += 1.0) {
            float py = hash1(i + floor(t * 0.5) + 30.0) * vert_range + effective_bottom;
            float px = effective_right + sin(t * (2.0 + i) + i * 3.14159 + 6.0) * 0.015 * burn_size;
            float spark_dist = length(vec2((uvpos.x - px) * inv_aspect, uvpos.y - py));
            ember_particle += smoothstep(0.02 * burn_size, 0.0, spark_dist) * (0.5 + 0.5 * sin(t * 10.0 + i * 5.0 + 3.0));
        }
        
        vec3 burn_line = vec3(0.8, 0.2, 0.0) * outer_glow 
                       + vec3(1.0, 0.6, 0.1) * inner_glow 
                       + vec3(1.0, 0.95, 0.7) * ember_core 
                       + vec3(1.0, 0.5, 0.0) * ember_particle;
        float burn_alpha = max(max(outer_glow, inner_glow), max(ember_core, ember_particle * 0.8));
        burn_alpha *= (0.9 + 0.2 * sin(t * 20.0 + edge_pos * 50.0 + 9.0) * sin(t * 17.0));
        burn_alpha *= progress_fade;
        
        burn_line_total += burn_line * burn_alpha;
        burn_alpha_total = max(burn_alpha_total, burn_alpha);
    }
    
    result.rgb += burn_line_total;
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
        auto padding = 50;
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
