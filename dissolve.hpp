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

// ============================================
// RIPPLE DISSOLUTION EFFECT
// ============================================

#define PI 3.14159265359

// Wave function: solution to 2D wave equation
float wave(vec2 uv, vec2 center, float time, float frequency, float speed) {
    float r = length(uv - center);
    if (r < 0.001) r = 0.001; 
    
    float k = 20.0 * PI * frequency;
    float omega = k * speed;
    float amplitude = 1.0 / sqrt(r + 0.1);
    
    return amplitude * cos(k * r - omega * time);
}

// Superposition of multiple wave sources
float wave_field(vec2 uv, float time, float freq_base) {
    float field = 0.0;
    
    vec2 sources[10];
    sources[0] = vec2(0.5, 0.5);
    sources[1] = vec2(0.2, 0.3);
    sources[2] = vec2(0.8, 0.3);
    sources[3] = vec2(0.3, 0.8);
    sources[4] = vec2(0.7, 0.7);
    sources[5] = vec2(0.1, 0.1);
    sources[6] = vec2(0.2, 0.7);
    sources[7] = vec2(0.9, 0.3);
    sources[8] = vec2(0.0, 0.5);
    sources[9] = vec2(0.7, 0.2);
    
    for (int i = 0; i < 10; i++) {
        float phase_offset = float(i) * 0.7;
        float freq = freq_base * (1.0 + float(i) * 0.1);
        field += wave(uv, sources[i], time + phase_offset, freq, flame_speed * 0.15);
    }
    
    return field / 10.0; 
}

// Gradient magnitude for edge glow
float gradient_magnitude(vec2 uv, float time, float freq) {
    float eps = 0.0005;
    float dudx = (wave_field(uv + vec2(eps, 0.0), time, freq) - 
                  wave_field(uv - vec2(eps, 0.0), time, freq)) / (2.0 * eps);
    float dudy = (wave_field(uv + vec2(0.0, eps), time, freq) - 
                  wave_field(uv - vec2(0.0, eps), time, freq)) / (2.0 * eps);
    return sqrt(dudx * dudx + dudy * dudy);
}

float smooth_threshold(float x, float edge, float width) {
    return 1.0 / (1.0 + exp(-(x - edge) / width));
}

// Soft edge vignette - fades to transparent near window boundaries
float edge_fade(vec2 uv, float softness) {
    float fade_left = smoothstep(0.0, softness, uv.x);
    float fade_right = smoothstep(0.0, softness, 1.0 - uv.x);
    float fade_bottom = smoothstep(0.0, softness, uv.y);
    float fade_top = smoothstep(0.0, softness, 1.0 - uv.y);
    
    return fade_left * fade_right * fade_bottom * fade_top;
}

void main()
{
    // Hard clip outside bounds
    if (uvpos.x < 0.0 || uvpos.x > 1.0 || uvpos.y < 0.0 || uvpos.y > 1.0) {
        gl_FragColor = vec4(0.0);
        return;
    }
    
    // Calculate soft edge fade
    float soft_edge = edge_fade(uvpos, 0.15);
    
    if (soft_edge < 0.001) {
        gl_FragColor = vec4(0.0);
        return;
    }
    
    // Sample original texture first
    vec4 original = get_pixel(uvpos);
    
    float effect_progress;
    float t;
    
 
        effect_progress = progress;
        t = progress * flame_speed * 0.1;
    
    
    vec2 uv = uvpos;
    uv.x *= size.x / size.y;
    
    float frequency = 1.0 + flame_width * 0.05 * (1.0 - progress);
    
    // Compute wave field
    float field = wave_field(uv, t, frequency);
    float grad = gradient_magnitude(uv, t, frequency);
    
    // DYNAMIC THRESHOLD based on effect_progress
    float threshold_min = -1.0;
    float threshold_max = 4.0;
    float dissolve_threshold = mix(threshold_min, threshold_max * (1.0 - progress), effect_progress);
    
    // Calculate mask
    float softness = 0.1 * max(0.01, flame_height);
    float dissolve = smooth_threshold(field, dissolve_threshold, softness);
    
    float effect_blend = smoothstep(0.0, 0.0, effect_progress);
    
    // Fade out glow as effect completes
    float glow_fadeout = 1.0 - smoothstep(0.85, 0.99, effect_progress);
    
    // Distortion ramps up then back down
    // Peak distortion at progress 0.5, zero at 0 and 1
    float distortion_strength = sin(progress * 3.14159);
    
    // Edge calculation for glow
    float edge_proximity = 2.0 - abs(field - dissolve_threshold) * 50.0;
    edge_proximity = clamp(edge_proximity, 0.0, 1.0);
    
    float edge_glow = edge_proximity * grad * 2.0 * effect_blend * glow_fadeout;
    
    // Distortion - peaks in middle, zero at start and end
    float distort_amount = field * 0.2 * distortion_strength;
    vec2 final_uv = uvpos + vec2(distort_amount);
    final_uv = clamp(final_uv, 0.0, 1.0);
    
    vec4 tex = get_pixel(final_uv);
    
    // Colors
    vec3 ripple_color = flame_color.rgb;
    vec3 glow_color = mix(ripple_color, vec3(1.0), 0.5); 
    
    // COMPOSITION
    vec4 result;
    
    float final_mask = mix(1.0, dissolve, effect_blend);
    
    result.rgb = mix(tex.rgb, glow_color * glow_fadeout, edge_glow * 0.6);
    
    float peak_glow = max(0.0, field - dissolve_threshold - 0.1) * 0.0001 * effect_blend * glow_fadeout;
    result.rgb += ripple_color * peak_glow * glow_fadeout;

    result.a = tex.a * final_mask;
    result.a = max(result.a, edge_glow * final_mask * glow_fadeout);
    
    gl_FragColor = clamp(result, 0.0, 1.0);
}
)";


/*
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

#define PI 3.14159265359
#define TAU 6.28318530718

vec2 to_polar(vec2 uv, vec2 center) {
    vec2 d = uv - center;
    return vec2(length(d), atan(d.y, d.x));
}

// Catenary sag: t in [0,1], returns sag amount
float catenary_sag(float t) {
    return 4.0 * t * (1.0 - t);
}

// Distance to nearest radial spoke
float dist_to_radial(float theta, float num_radials, float r) {
    float sector = TAU / num_radials;
    float a = mod(theta, sector);
    float angular_dist = min(a, sector - a);
    return angular_dist * r;
}

// Spider web with build animation
// build_progress: 0 = nothing, 1 = complete web
// Spider web with build animation
// build_progress: 0 = nothing, 1 = complete web
float spider_web(vec2 uv, vec2 center, float num_radials, float num_spirals, float build_progress, float time) {
    vec2 polar = to_polar(uv, center);
    float r = polar.x;
    float theta = polar.y + PI;
    
    float web = 0.0;
    float thread = 0.0015;
    
    // === BUILD PHASES ===
    // Phase 1 (0.0 - 0.3): Radials grow outward from center
    // Phase 2 (0.2 - 1.0): Spiral builds outward, following radials
    
    float radial_progress = smoothstep(0.0, 0.35, build_progress);
    float spiral_progress = smoothstep(0.15, 1.0, build_progress);
    
    // === RADIAL SPOKES ===
    // Grow outward from center based on radial_progress
// === RADIAL SPOKES ===
    // Grow outward from center based on radial_progress
    float radial_max_r = radial_progress * 0.95;
    
    // Radials appear one at a time
    float radial_index = floor(mod(theta, TAU) / (TAU / num_radials));
    float radial_order = mod(radial_index * 7.0, num_radials); // Shuffle order using prime
    float radial_start_time = radial_order / num_radials;
    float this_radial_progress = smoothstep(radial_start_time, radial_start_time + 0.15, radial_progress);
    
    // Add slight stagger - different radials grow at slightly different speeds
    float radial_stagger = sin(radial_index * 1.7) * 0.05;
    float this_radial_max = this_radial_progress * 0.95 + radial_stagger * this_radial_progress;
    
    // Random broken radial - 5% chance per radial
    float radial_seed = fract(sin(radial_index * 127.1) * 43758.5453);
    float radial_broken = step(0.95, radial_seed);
    float radial_break_point = fract(sin(radial_index * 311.7) * 18732.1) * 0.5 + 0.3;
    
    float radial_dist = dist_to_radial(theta, num_radials, r);
    float radials = 1.0 - smoothstep(0.0, thread, radial_dist);
    radials *= step(0.015, r); // Start from hub
    radials *= 1.0 - smoothstep(this_radial_max - 0.02, this_radial_max, r); // Growing edge
    
    // Apply broken radial - cut thread at break point
    radials *= 1.0 - radial_broken * step(radial_break_point, r) * (1.0 - step(radial_break_point + 0.15, r));
    
    // Bright tip at growing end
    float radial_tip = smoothstep(this_radial_max - 0.06, this_radial_max - 0.01, r);
    radial_tip *= (1.0 - smoothstep(this_radial_max - 0.01, this_radial_max, r));
    radial_tip *= (1.0 - smoothstep(0.0, thread * 2.0, radial_dist));
    
    web = max(web, radials);
    
// === SPIRAL SILK - continuous curved connections ===
    float indent = 0.005;
    float mask_size = 0.85;
    
    float ang = num_radials * atan(uv.y - center.y, uv.x - center.x);
    float spiral_col = sin(r) + (r + 0.1) * abs(sin(ang * 0.5) * indent);
    float spiral_mask = step(spiral_col, PI * mask_size);
    
    // Build animation - spiral expands outward
    float max_visible_r = spiral_progress * mask_size * 1.2;
    
    // Single spiral thread
    float spiral_pattern = fract(spiral_col * num_spirals + ang * 0.0095);
    
    // Random broken segments - 5% chance
    float segment_index = floor(spiral_col * num_spirals);
    float sector_index = floor(mod(ang, TAU) / (TAU / num_radials));
    float spiral_seed = fract(sin(segment_index * 93.7 + sector_index * 45.3) * 28461.2);
    float spiral_broken = step(0.95, spiral_seed);
    
    // Single thread line
    float spiral_thread = smoothstep(0.94, 0.97, spiral_pattern) - smoothstep(0.97, 1.0, spiral_pattern);
    
    // Apply broken segments
    spiral_thread *= 1.0 - spiral_broken;
    spiral_thread *= spiral_mask;
    spiral_thread *= step(r, max_visible_r);
    
    // Bright glow at the drawing edge
    float edge_dist = abs(r - max_visible_r);
    float drawing_glow = 1.0 - smoothstep(0.0, 0.04, edge_dist);
    drawing_glow *= spiral_thread;
    drawing_glow *= step(0.15, build_progress);
    drawing_glow *= 1.0 - step(0.95, spiral_progress);
    
    web = max(web, spiral_thread);
    web = max(web, drawing_glow * 1.5);
    

    
    // === ANCHOR THREADS - appear with radials ===
    float anchor_angles[5];
    anchor_angles[0] = -0.3;
    anchor_angles[1] = 0.9;
    anchor_angles[2] = 2.1;
    anchor_angles[3] = -1.7;
    anchor_angles[4] = 2.9;
    
    for (int i = 0; i < 5; i++) {
        float target_angle = anchor_angles[i];
        float angle_diff = abs(polar.y - target_angle);
        angle_diff = min(angle_diff, TAU - angle_diff);
        
        float anchor_max_r = radial_progress * 1.3;
        
        float anchor_thread = 1.0 - smoothstep(0.0, thread * 1.2, angle_diff * r);
        anchor_thread *= smoothstep(0.85, 0.88, r);
        anchor_thread *= 1.0 - smoothstep(anchor_max_r - 0.05, anchor_max_r, r);
        
        web = max(web, anchor_thread);
    }
    
    // Dew droplets appear last
    float dew_progress = smoothstep(0.7, 1.0, build_progress);
    float dew_pattern = sin(theta * 80.0) * sin(r * 500.0);
    float dew = step(0.93, dew_pattern) * web * 0.4 * dew_progress;
    web = max(web, dew);
    
    // Add glow at radial tips
    web = max(web, radial_tip * 1.5 * (1.0 - step(0.99, radial_progress)));
    
    return web;
}

float edge_fade(vec2 uv, float softness) {
    float fade_left = smoothstep(0.0, softness, uv.x);
    float fade_right = smoothstep(0.0, softness, 1.0 - uv.x);
    float fade_bottom = smoothstep(0.0, softness, uv.y);
    float fade_top = smoothstep(0.0, softness, 1.0 - uv.y);
    return fade_left * fade_right * fade_bottom * fade_top;
}

void main()
{
    if (uvpos.x < 0.0 || uvpos.x > 1.0 || uvpos.y < 0.0 || uvpos.y > 1.0) {
        gl_FragColor = vec4(0.0);
        return;
    }
    
    float soft_edge = edge_fade(uvpos, 0.15);
    if (soft_edge < 0.001) {
        gl_FragColor = vec4(0.0);
        return;
    }
    
    vec4 original = get_pixel(uvpos);
    
    // Build progress based on direction
    // direction == 0 (opening): build web from nothing to full
    // direction == 1 (closing): reverse - full web to nothing
    float build_progress;
    float t;
    float fade_out;
    
    if (direction == 0) {
        // OPENING: Web builds up, then fades out after completion
        // progress 0.0 - 0.7: build the web (build_progress 0 -> 1)
        // progress 0.7 - 0.85: hold at full (brief pause)
        // progress 0.85 - 1.0: fade out
        build_progress = smoothstep(0.0, 0.4, progress);
        t = progress * flame_speed * 2.0;
        
        // Fade out after build completes
        fade_out = 1.0 - smoothstep(0.85, 1.0, progress);
    } else {
        // CLOSING: Web builds up, then fades out after completion
        // progress 0.0 - 0.7: build the web (build_progress 0 -> 1)
        // progress 0.7 - 0.85: hold at full (brief pause)
        // progress 0.85 - 1.0: fade out
        build_progress = smoothstep(0.0, 0.4, progress);
        t = progress * flame_speed * 2.0;
        
        // Fade out after build completes
        fade_out = 1.0 - smoothstep(0.85, 1.0, progress);
    }
    
    // Coordinate transform
    vec2 uv = uvpos;
    float aspect = size.x / size.y;
    uv = uv - 0.5;
    uv.x *= aspect;
    
//    vec2 web_center = vec2(0.0, 0.0);
     vec2 web_center = vec2(-0.18 * aspect, 0.1);
    
    // Web parameters
    float num_radials = 28.0 + flame_width * 0.2;
    float num_spirals = 25.0 + flame_height * 0.25;
    
    // Wind displacement using calculus - sine wave with derivative for velocity
    float wind_time = t * 1.5;
    
    // Base wind function: f(t) = sin(t) + 0.5*sin(2.3t)
    // Derivative f'(t) = cos(t) + 1.15*cos(2.3t) gives velocity for motion blur feel
    float wind_phase = sin(wind_time) + 0.5 * sin(wind_time * 2.3);
    float wind_velocity = cos(wind_time) + 1.15 * cos(wind_time * 2.3);
    
    // Displacement varies with distance from center (more sway at edges)
    // Using r^2 for quadratic falloff from center (integral of 2r)
    float r_from_center = length(uv - web_center);
    float displacement_strength = r_from_center * r_from_center * 0.15;
    
    // Apply wind - horizontal displacement modulated by vertical position
    // Second derivative: f''(t) = -sin(t) - 2.645*sin(2.3t) for acceleration sway
    float wind_accel = -sin(wind_time) - 2.645 * sin(wind_time * 2.3);
    
    vec2 wind_offset;
    wind_offset.x = wind_phase * displacement_strength;
    wind_offset.y = wind_velocity * displacement_strength * 0.3 + wind_accel * displacement_strength * 0.1;
    
    // Damped harmonic motion for natural decay: e^(-kt) * sin(wt)
    float damping = exp(-r_from_center * 0.5);
    wind_offset *= damping;
    
    vec2 displaced_uv = uv - wind_offset;
    
    float web = spider_web(displaced_uv, web_center, num_radials, num_spirals, build_progress, t);
    
    // Visibility based on build progress and fade out
    float effect_blend = smoothstep(0.0, 0.05, build_progress) * fade_out;
    
    // WHITE silk colors
    vec3 silk_color = vec3(0.82, 0.84, 0.88);
    vec3 glow_color = vec3(1.0);
    
    // Composition
    vec4 result;
    vec4 tex = original;
    
    result.rgb = tex.rgb;
    
    // White silk threads
    float thread_vis = web * effect_blend * 0.85;
    result.rgb = mix(result.rgb, silk_color, thread_vis);
    
    // Extra glow during construction
    float construction_glow = web * effect_blend * 0.2 * (1.0 - build_progress);
    result.rgb += glow_color * construction_glow;
    
    // Alpha - web over original
    result.a = tex.a;
    result.a = max(result.a, thread_vis * 0.6);
    
    result.rgb = mix(tex.rgb, result.rgb, soft_edge);
    result.a = mix(tex.a, result.a, soft_edge);
    
    gl_FragColor = clamp(result, 0.0, 1.0);
}
)";
*/    
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
