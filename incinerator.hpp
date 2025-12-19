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
    
    for (int iter = 0; iter < 40; iter++)
    {
        vec3 p = z * normalize(vec3(I + I, 0.0) - iResolution.xyy);
        p.z += 5.0 + cos(t);
        
        float angle = p.y * 0.5;
        mat2 m = mat2(cos(angle), cos(angle + 33.0), cos(angle + 11.0), cos(angle));
        p.xz = m * p.xz / max(p.y * 0.1 + 1.0, 0.1);
        
        for (d = 2.0; d < 15.0; d /= 0.6)
        {
            p += cos((p.yzx - vec3(t / 0.1, t, d)) * d) / d;
        }
        
        d = 0.01 + abs(length(p.xz) + p.y * 0.3 - 0.5) / 7.0;
        z += d;
        
        if (d < 0.80) O += (sin(z / 3.0 + vec4(7.0, 2.0, 3.0, 0.0)) + 1.1) / d;
    }
    return tanh_approx(O / 1000.0);
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

// Helper function to accumulate fire from nearby columns
// pixel_pos: current pixel position in screen space
// spacing: distance between fire columns
// half_spacing: spacing * 0.5
// shift: phase shift for this layer
// width_check: maximum distance to consider
// fireY: vertical position in fire space
// iResolution: fire resolution
// t: time
// time_offset: time offset for this edge
vec4 accumulate_nearby_fire(float pixel_pos, float spacing, float half_spacing, float shift,
                            float width_check, float fireY, vec3 iResolution, float t, float time_offset)
{
    vec4 accum = vec4(0.0);
    
    // Calculate which column index this pixel is closest to
    // Columns are at positions: neg_spacing_2 + spacing * shift + n * spacing
    // where neg_spacing_2 = -spacing * 2.0
    float base_offset = -spacing * 2.0 + spacing * shift;
    
    // Find the column index nearest to this pixel
    float col_float = (pixel_pos - base_offset - half_spacing) / spacing;
    int col_center = int(floor(col_float + 0.5));
    
    // Check only the 3 nearest columns (center and neighbors)
    for (int delta = -1; delta <= 1; delta++)
    {
        int col = col_center + delta;
        float off = base_offset + float(col) * spacing;
        float localX = pixel_pos - off - half_spacing;
        
        if (abs(localX) < width_check)
        {
            accum += render_fire(vec2(localX, fireY), iResolution, t + off * 0.01 + time_offset) * 0.5;
        }
    }
    
    return accum;
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

    


vec4 fire_color_accum = vec4(0.0);
    vec4 fire_bottom = vec4(0.0);
    vec4 fire_top = vec4(0.0);
    vec4 fire_left = vec4(0.0);
    vec4 fire_right = vec4(0.0);
    float a_3d = 0.0;

    // End blur factor - increases blur near the end of animation
    float end_blur = smoothstep(0.75, 0.95, progress);
    float blur_radius = end_blur * 0.015;

    // ============ 3D FIRE - FROM ALL SIDES ============
    // Only render fire near the boundary, and only on the unburned side
    if (dist_from_burn >= -0.05 && dist_from_burn <= 0.5)
    {
        float fire_scale = 0.2 + flame_height;
        float fire_width_scale = max(width * 0.12, 5.0) * (1.5 + flame_width);
        float fire_res = max(height * 0.15, 0.01);
        vec3 iResolution = vec3(fire_width_scale, fire_res, fire_res);
        float spacing = fire_width_scale * 0.5;
        
        // Pre-compute arrays - keeping exact original values
        float offsets[6];
        offsets[0] = 0.0; offsets[1] = 1.5; offsets[2] = 3.0; 
        offsets[3] = 4.5; offsets[4] = 4.5; offsets[5] = 4.5;
        
        float shifts[6];
        shifts[0] = 0.0;  shifts[1] = 0.25; shifts[2] = 0.5;
        shifts[3] = 0.75; shifts[4] = 0.65; shifts[5] = 0.35;

        // Pre-compute common values
        float half_spacing = spacing * 0.5;

        // Fire from bottom edge - only if this pixel is within horizontal bounds
// Fire from bottom edge - soft blend instead of hard clip
if (dist_from_bottom >= -0.05 && dist_from_bottom <= 0.3) {
    float h_blend = smoothstep(effective_left - 0.05, effective_left + 0.05, uvpos.x) 
                  * smoothstep(effective_right + 0.05, effective_right - 0.05, uvpos.x);
    
    float fireY = (dist_from_bottom - 0.05) * height * fire_scale;
    float pixel_pos = uvpos.x * width;
    pixel_pos += (hash2(uvpos * 100.0 + t) - 0.5) * spacing * 0.01;
    for(int i = 0; i < 6; i++) {
        float width_check = fire_width_scale * (i < 2 ? 1.0 : 1.5);
        fire_bottom += accumulate_nearby_fire(pixel_pos, spacing, half_spacing, shifts[i],
                                               width_check, fireY, iResolution, t, offsets[i]);
    }
    fire_bottom *= h_blend;
}

// Fire from top edge
if (dist_from_top >= -0.05 && dist_from_top <= 0.3) {
    float h_blend = smoothstep(effective_left - 0.05, effective_left + 0.05, uvpos.x) 
                  * smoothstep(effective_right + 0.05, effective_right - 0.05, uvpos.x);
    
    float fireY = (dist_from_top - 0.05) * height * fire_scale;
    float pixel_pos = uvpos.x * width;
    pixel_pos += (hash2(uvpos * 100.0 + t) - 0.5) * spacing * 0.01;
    for(int i = 0; i < 6; i++) {
        float width_check = fire_width_scale * (i < 2 ? 1.0 : 1.5);
        fire_top += accumulate_nearby_fire(pixel_pos, spacing, half_spacing, shifts[i],
                                            width_check, fireY, iResolution, t, offsets[i] + 20.0);
    }
    fire_top *= h_blend;
}

// Fire from left edge
if (dist_from_left >= -0.05 && dist_from_left <= 0.3) {
    float v_blend = smoothstep(effective_bottom - 0.05, effective_bottom + 0.05, uvpos.y) 
                  * smoothstep(effective_top + 0.05, effective_top - 0.05, uvpos.y);
    
    float fireY = (dist_from_left - 0.05) * width * fire_scale;
    float pixel_pos = uvpos.y * height;
    pixel_pos += (hash2(uvpos * 100.0 + t * 1.1) - 0.5) * spacing * 0.01;
    for(int i = 0; i < 6; i++) {
        float width_check = fire_width_scale * (i < 2 ? 1.0 : 1.5);
        fire_left += accumulate_nearby_fire(pixel_pos, spacing, half_spacing, shifts[i],
                                             width_check, fireY, iResolution, t, offsets[i] + 40.0);
    }
    fire_left *= v_blend;
}

// Fire from right edge
if (dist_from_right >= -0.05 && dist_from_right <= 0.3) {
    float v_blend = smoothstep(effective_bottom - 0.05, effective_bottom + 0.05, uvpos.y) 
                  * smoothstep(effective_top + 0.05, effective_top - 0.05, uvpos.y);
    
    float fireY = (dist_from_right - 0.05) * width * fire_scale;
    float pixel_pos = uvpos.y * height;
    pixel_pos += (hash2(uvpos * 100.0 + t * 1.1) - 0.5) * spacing * 0.01;
    for(int i = 0; i < 6; i++) {
        float width_check = fire_width_scale * (i < 2 ? 1.0 : 1.5);
        fire_right += accumulate_nearby_fire(pixel_pos, spacing, half_spacing, shifts[i],
                                              width_check, fireY, iResolution, t, offsets[i] + 60.0);
    }
    fire_right *= v_blend;
}
        

// Take maximum of all edges instead of accumulating to prevent bright overlap
        fire_color_accum = max(max(fire_bottom, fire_top), max(fire_left, fire_right));
        fire_color_accum = clamp(fire_color_accum, 0.0, 1.0);
        
        // Apply blur softening at the end
        if (end_blur > 0.0) {
            // Soften the fire by blending with nearby samples
            vec4 blur_accum = fire_color_accum;
            float blur_samples = 1.0;
            for (float bx = -1.0; bx <= 1.0; bx += 1.0) {
                for (float by = -1.0; by <= 1.0; by += 1.0) {
                    if (bx != 0.0 || by != 0.0) {
                        vec2 offset = vec2(bx, by) * blur_radius;
                        // Approximate blur by softening the accumulated color
                        blur_accum += fire_color_accum * exp(-length(vec2(bx, by)) * 0.5);
                        blur_samples += exp(-length(vec2(bx, by)) * 0.5);
                    }
                }
            }
            fire_color_accum = mix(fire_color_accum, blur_accum / blur_samples, end_blur);
        }
        
        a_3d = (fire_color_accum.r + fire_color_accum.g + fire_color_accum.b) / 3.0;
        // Soften the alpha threshold at the end for blur effect
        float alpha_low = mix(0.45, 0.25, end_blur);
        float alpha_high = mix(0.9, 0.6, end_blur);
        a_3d = smoothstep(alpha_low, alpha_high, a_3d);
        a_3d *= clamp(progress * 10.0, 0.0, 1.0);
        
        // Fade out fire opacity at the very end
        a_3d *= smoothstep(1.0, 0.85, progress);
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
    
// ====== FIRE SHADOW/SMOKE - adds contrast against light backgrounds ======
if (inside_unburned) {
    // Create a dark shadow zone that extends slightly beyond the fire
    float shadow_zone = smoothstep(0.25, 0.0, dist_from_burn);
    
    // Shadow is strongest where fire is, fades out beyond
    float shadow_intensity = max(a_3d * 1.2, shadow_zone * 0.6);
    shadow_intensity = clamp(shadow_intensity, 0.0, 0.85);
    
    // Shadow timing - matches fire fade in/out
    float shadow_delay = 0.05;
    float shadow_fade_duration = 0.15;
    float shadow_progress_factor;
    if (direction == 1) {
        // Reverse: shadow fades out as fire disappears
        shadow_progress_factor = smoothstep(shadow_delay + shadow_fade_duration, shadow_delay, 1.0 - progress);
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

// ====== Charred edge - only inside unburned area ======
if (inside_unburned) {
    float char_zone = smoothstep(0.0, 0.03, dist_from_burn) * smoothstep(0.06, 0.03, dist_from_burn);
    vec3 char_color = vec3(0.1, 0.05, 0.0);
    
    // Char fades in AFTER fire is established (delayed by 0.1 progress)
    // and fades out BEFORE fire disappears during reverse
    float char_delay = 0.1;
    float char_fade_duration = 0.15;
    float char_progress_factor;
    if (direction == 1) {
        // Reverse: char fades out first (at higher progress values)
        char_progress_factor = smoothstep(char_delay + char_fade_duration, char_delay, 1.0 - progress);
    } else {
        // Forward: char fades in after fire is established
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
    
    // Blue visibility based on burn_progress (already direction-corrected)
    float blue_delay = 0.15;
    float blue_fade_duration = 0.2;
    
    // Use burn_progress which is already flipped for reverse direction
    float blue_progress_factor = smoothstep(blue_delay, blue_delay + blue_fade_duration, burn_progress);
    // Fade out near the end
    blue_progress_factor *= smoothstep(1.0, 0.85, burn_progress);
    
    blue_zone *= blue_progress_factor;
    
    vec3 blue_color = mix(vec3(0.0, 0.2, 0.8), vec3(0.3, 0.5, 1.0), smoothstep(blue_start, blue_start + blue_height * 0.7, dist_from_burn));
    bg.rgb = mix(bg.rgb, blue_color, blue_zone * 0.85);
}

// Composite 3D fire with end blur
    vec4 result = vec4(fire_color_accum.rgb, 1.0) * a_3d + bg * (1.0 - a_3d);
    
    // Soften fire colors at the end
    if (end_blur > 0.0) {
        vec3 soft_fire = mix(result.rgb, (result.rgb + bg.rgb) * 0.5, end_blur * 0.3);
        result.rgb = mix(result.rgb, soft_fire, a_3d);
    }
    
    // ============ BURN LINE - ALL SIDES ============
    // Only render burn lines on the boundary of the unburned area
    float burn_size = 4.0 * (1.0 + end_blur * 2.0);  // Expand burn line at end for blur
    
    vec3 burn_line_total = vec3(0.0);
    float burn_alpha_total = 0.0;
    
    // Pre-compute common values for burn lines
    float progress_fade = clamp(progress * 10.0, 0.0, 1.0);
    float inv_aspect = width / height;
    
    // Bottom edge burn line - only if within horizontal bounds of unburned area
    if (uvpos.x >= effective_left && uvpos.x <= effective_right) {
        float edge_dist = dist_from_bottom;
        float edge_pos = uvpos.x;
        
        float wave = sin(edge_pos * 40.0 + t * 2.0) * 0.003 
                   + sin(edge_pos * 80.0 - t * 3.0) * 0.002;
        float edge_noise = hash2(vec2(floor(edge_pos * width * 0.5), floor(t * 2.0))) * 0.01;
        float adjusted_dist = edge_dist + wave + edge_noise;
        
        float abs_adj = abs(adjusted_dist);
        float ember_core = smoothstep(0.012 * burn_size, 0.0, abs_adj) 
                         * smoothstep(-0.005 * burn_size, 0.005 * burn_size, adjusted_dist);
        float inner_glow = smoothstep(0.025 * burn_size, 0.0, abs_adj) 
                         * smoothstep(-0.01 * burn_size, 0.01 * burn_size, adjusted_dist);
        float outer_glow = smoothstep(0.05 * burn_size, 0.0, abs_adj) 
                         * smoothstep(-0.02 * burn_size, 0.02 * burn_size, adjusted_dist);
        
        // Orange ember particles
        float ember_particle = 0.0;
        for (float i = 0.0; i < 3.0; i += 1.0) {
            float px = hash1(i + floor(t * 0.5)) * (effective_right - effective_left) + effective_left;
            float local_wave = calc_wave_offset(px, t, wave_fade);
            float py = effective_bottom + sin(t * (2.0 + i) + i * 3.14159) * 0.015 * burn_size;
            float spark_dist = length(vec2((uvpos.x - px) * inv_aspect, uvpos.y - py));
            ember_particle += smoothstep(0.02 * burn_size, 0.0, spark_dist) * (0.5 + 0.5 * sin(t * 10.0 + i * 5.0));
        }
        
        // Red ember particles - different timing and positions
        float red_ember_particle = 0.0;
        for (float i = 0.0; i < 4.0; i += 1.0) {
            float px = hash1(i + floor(t * 0.4) + 100.0) * (effective_right - effective_left) + effective_left;
            float py = effective_bottom + sin(t * (1.5 + i * 0.7) + i * 2.718) * 0.018 * burn_size;
            float spark_dist = length(vec2((uvpos.x - px) * inv_aspect, uvpos.y - py));
            red_ember_particle += smoothstep(0.025 * burn_size, 0.0, spark_dist) * (0.4 + 0.6 * sin(t * 8.0 + i * 4.0));
        }
        
        vec3 burn_line = vec3(0.8, 0.2, 0.0) * outer_glow 
                       + vec3(1.0, 0.6, 0.1) * inner_glow 
                       + vec3(1.0, 0.95, 0.7) * ember_core 
                       + vec3(1.0, 0.5, 0.0) * ember_particle
                       + vec3(1.0, 0.1, 0.0) * red_ember_particle;  // Deep red embers
        float burn_alpha = max(max(outer_glow, inner_glow), max(ember_core, max(ember_particle * 0.8, red_ember_particle * 0.9)));
        burn_alpha *= (0.9 + 0.2 * sin(t * 20.0 + edge_pos * 50.0) * sin(t * 17.0));
        burn_alpha *= progress_fade;
        
        burn_line_total += burn_line * burn_alpha;
        burn_alpha_total = max(burn_alpha_total, burn_alpha);
    }
    
    // Top edge burn line - only if within horizontal bounds of unburned area
    if (uvpos.x >= effective_left && uvpos.x <= effective_right) {
        float edge_dist = dist_from_top;
        float edge_pos = uvpos.x;
        
        float wave = sin(edge_pos * 40.0 + t * 2.0 + 5.0) * 0.003 
                   + sin(edge_pos * 80.0 - t * 3.0 + 7.0) * 0.002;
        float edge_noise = hash2(vec2(floor(edge_pos * width * 0.5), floor(t * 2.0) + 1.0)) * 0.01;
        float adjusted_dist = edge_dist + wave + edge_noise;
        
        float abs_adj = abs(adjusted_dist);
        float ember_core = smoothstep(0.012 * burn_size, 0.0, abs_adj) 
                         * smoothstep(-0.005 * burn_size, 0.005 * burn_size, adjusted_dist);
        float inner_glow = smoothstep(0.025 * burn_size, 0.0, abs_adj) 
                         * smoothstep(-0.01 * burn_size, 0.01 * burn_size, adjusted_dist);
        float outer_glow = smoothstep(0.05 * burn_size, 0.0, abs_adj) 
                         * smoothstep(-0.02 * burn_size, 0.02 * burn_size, adjusted_dist);
        
        // Orange ember particles
        float ember_particle = 0.0;
        for (float i = 0.0; i < 3.0; i += 1.0) {
            float px = hash1(i + floor(t * 0.5) + 10.0) * (effective_right - effective_left) + effective_left;
            float py = effective_top + sin(t * (2.0 + i) + i * 3.14159 + 2.0) * 0.015 * burn_size;
            float spark_dist = length(vec2((uvpos.x - px) * inv_aspect, uvpos.y - py));
            ember_particle += smoothstep(0.02 * burn_size, 0.0, spark_dist) * (0.5 + 0.5 * sin(t * 10.0 + i * 5.0 + 1.0));
        }
        
        // Red ember particles - different timing and positions
        float red_ember_particle = 0.0;
        for (float i = 0.0; i < 4.0; i += 1.0) {
            float px = hash1(i + floor(t * 0.4) + 110.0) * (effective_right - effective_left) + effective_left;
            float py = effective_top + sin(t * (1.5 + i * 0.7) + i * 2.718 + 1.5) * 0.018 * burn_size;
            float spark_dist = length(vec2((uvpos.x - px) * inv_aspect, uvpos.y - py));
            red_ember_particle += smoothstep(0.025 * burn_size, 0.0, spark_dist) * (0.4 + 0.6 * sin(t * 8.0 + i * 4.0 + 1.0));
        }
        
        vec3 burn_line = vec3(0.8, 0.2, 0.0) * outer_glow 
                       + vec3(1.0, 0.6, 0.1) * inner_glow 
                       + vec3(1.0, 0.95, 0.7) * ember_core 
                       + vec3(1.0, 0.5, 0.0) * ember_particle
                       + vec3(1.0, 0.1, 0.0) * red_ember_particle;  // Deep red embers
        float burn_alpha = max(max(outer_glow, inner_glow), max(ember_core, max(ember_particle * 0.8, red_ember_particle * 0.9)));
        burn_alpha *= (0.9 + 0.2 * sin(t * 20.0 + edge_pos * 50.0 + 3.0) * sin(t * 17.0));
        burn_alpha *= progress_fade;
        
        burn_line_total += burn_line * burn_alpha;
        burn_alpha_total = max(burn_alpha_total, burn_alpha);
    }
    
    // Left edge burn line - only if within vertical bounds of unburned area
    if (uvpos.y >= effective_bottom && uvpos.y <= effective_top) {
        float edge_dist = dist_from_left;
        float edge_pos = uvpos.y;
        
        float wave = sin(edge_pos * 40.0 + t * 2.0 + 10.0) * 0.003 
                   + sin(edge_pos * 80.0 - t * 3.0 + 14.0) * 0.002;
        float edge_noise = hash2(vec2(floor(edge_pos * height * 0.5), floor(t * 2.0) + 2.0)) * 0.01;
        float adjusted_dist = edge_dist + wave + edge_noise;
        
        float abs_adj = abs(adjusted_dist);
        float ember_core = smoothstep(0.012 * burn_size, 0.0, abs_adj) 
                         * smoothstep(-0.005 * burn_size, 0.005 * burn_size, adjusted_dist);
        float inner_glow = smoothstep(0.025 * burn_size, 0.0, abs_adj) 
                         * smoothstep(-0.01 * burn_size, 0.01 * burn_size, adjusted_dist);
        float outer_glow = smoothstep(0.05 * burn_size, 0.0, abs_adj) 
                         * smoothstep(-0.02 * burn_size, 0.02 * burn_size, adjusted_dist);
        
        // Orange ember particles
        float ember_particle = 0.0;
        for (float i = 0.0; i < 3.0; i += 1.0) {
            float py = hash1(i + floor(t * 0.5) + 20.0) * (effective_top - effective_bottom) + effective_bottom;
            float px = effective_left + sin(t * (2.0 + i) + i * 3.14159 + 4.0) * 0.015 * burn_size;
            float spark_dist = length(vec2((uvpos.x - px) * inv_aspect, uvpos.y - py));
            ember_particle += smoothstep(0.02 * burn_size, 0.0, spark_dist) * (0.5 + 0.5 * sin(t * 10.0 + i * 5.0 + 2.0));
        }
        
        // Red ember particles - different timing and positions
        float red_ember_particle = 0.0;
        for (float i = 0.0; i < 4.0; i += 1.0) {
            float py = hash1(i + floor(t * 0.4) + 120.0) * (effective_top - effective_bottom) + effective_bottom;
            float px = effective_left + sin(t * (1.5 + i * 0.7) + i * 2.718 + 3.0) * 0.018 * burn_size;
            float spark_dist = length(vec2((uvpos.x - px) * inv_aspect, uvpos.y - py));
            red_ember_particle += smoothstep(0.025 * burn_size, 0.0, spark_dist) * (0.4 + 0.6 * sin(t * 8.0 + i * 4.0 + 2.0));
        }
        
        vec3 burn_line = vec3(0.8, 0.2, 0.0) * outer_glow 
                       + vec3(1.0, 0.6, 0.1) * inner_glow 
                       + vec3(1.0, 0.95, 0.7) * ember_core 
                       + vec3(1.0, 0.5, 0.0) * ember_particle
                       + vec3(1.0, 0.1, 0.0) * red_ember_particle;  // Deep red embers
        float burn_alpha = max(max(outer_glow, inner_glow), max(ember_core, max(ember_particle * 0.8, red_ember_particle * 0.9)));
        burn_alpha *= (0.9 + 0.2 * sin(t * 20.0 + edge_pos * 50.0 + 6.0) * sin(t * 17.0));
        burn_alpha *= progress_fade;
        
        burn_line_total += burn_line * burn_alpha;
        burn_alpha_total = max(burn_alpha_total, burn_alpha);
    }
    
    // Right edge burn line - only if within vertical bounds of unburned area
    if (uvpos.y >= effective_bottom && uvpos.y <= effective_top) {
        float edge_dist = dist_from_right;
        float edge_pos = uvpos.y;
        
        float wave = sin(edge_pos * 40.0 + t * 2.0 + 15.0) * 0.003 
                   + sin(edge_pos * 80.0 - t * 3.0 + 21.0) * 0.002;
        float edge_noise = hash2(vec2(floor(edge_pos * height * 0.5), floor(t * 2.0) + 3.0)) * 0.01;
        float adjusted_dist = edge_dist + wave + edge_noise;
        
        float abs_adj = abs(adjusted_dist);
        float ember_core = smoothstep(0.012 * burn_size, 0.0, abs_adj) 
                         * smoothstep(-0.005 * burn_size, 0.005 * burn_size, adjusted_dist);
        float inner_glow = smoothstep(0.025 * burn_size, 0.0, abs_adj) 
                         * smoothstep(-0.01 * burn_size, 0.01 * burn_size, adjusted_dist);
        float outer_glow = smoothstep(0.05 * burn_size, 0.0, abs_adj) 
                         * smoothstep(-0.02 * burn_size, 0.02 * burn_size, adjusted_dist);
        
        // Orange ember particles
        float ember_particle = 0.0;
        for (float i = 0.0; i < 3.0; i += 1.0) {
            float py = hash1(i + floor(t * 0.5) + 30.0) * (effective_top - effective_bottom) + effective_bottom;
            float px = effective_right + sin(t * (2.0 + i) + i * 3.14159 + 6.0) * 0.015 * burn_size;
            float spark_dist = length(vec2((uvpos.x - px) * inv_aspect, uvpos.y - py));
            ember_particle += smoothstep(0.02 * burn_size, 0.0, spark_dist) * (0.5 + 0.5 * sin(t * 10.0 + i * 5.0 + 3.0));
        }
        
        // Red ember particles - different timing and positions
        float red_ember_particle = 0.0;
        for (float i = 0.0; i < 4.0; i += 1.0) {
            float py = hash1(i + floor(t * 0.4) + 130.0) * (effective_top - effective_bottom) + effective_bottom;
            float px = effective_right + sin(t * (1.5 + i * 0.7) + i * 2.718 + 4.5) * 0.018 * burn_size;
            float spark_dist = length(vec2((uvpos.x - px) * inv_aspect, uvpos.y - py));
            red_ember_particle += smoothstep(0.025 * burn_size, 0.0, spark_dist) * (0.4 + 0.6 * sin(t * 8.0 + i * 4.0 + 3.0));
        }
        
        vec3 burn_line = vec3(0.8, 0.2, 0.0) * outer_glow 
                       + vec3(1.0, 0.6, 0.1) * inner_glow 
                       + vec3(1.0, 0.95, 0.7) * ember_core 
                       + vec3(1.0, 0.5, 0.0) * ember_particle
                       + vec3(1.0, 0.1, 0.0) * red_ember_particle;  // Deep red embers
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
