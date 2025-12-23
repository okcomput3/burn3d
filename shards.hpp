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
// PHOTOREALISTIC SPIDER WEB GLASS SHATTER
// Combines web geometry with realistic physics
// ============================================

#define PI 3.14159265359
#define TAU 6.28318530718

// Physical Constants
#define IOR 1.45
#define DISPERSION 0.025
#define SHARD_THICKNESS 0.0

// =============================================
// NOISE & HASH
// =============================================

float hash(vec2 p) {
    return fract(sin(dot(p, vec2(127.1, 311.7))) * 43758.5453);
}

vec2 hash2(vec2 p) {
    return vec2(
        fract(sin(dot(p, vec2(127.1, 311.7))) * 43758.5453),
        fract(sin(dot(p, vec2(269.5, 183.3))) * 17632.8491)
    );
}

vec3 hash3(vec2 p) {
    vec3 q = vec3(dot(p, vec2(127.1, 311.7)), 
                  dot(p, vec2(269.5, 183.3)), 
                  dot(p, vec2(419.2, 371.9)));
    return fract(sin(q) * 43758.5453);
}

// =============================================
// MATH & TRANSFORMS
// =============================================

vec2 to_polar(vec2 uv, vec2 center) {
    vec2 d = uv - center;
    return vec2(length(d), atan(d.y, d.x));
}

vec3 rotate_x(vec3 p, float angle) {
    float c = cos(angle), s = sin(angle);
    return vec3(p.x, p.y * c - p.z * s, p.y * s + p.z * c);
}

vec3 rotate_y(vec3 p, float angle) {
    float c = cos(angle), s = sin(angle);
    return vec3(p.x * c + p.z * s, p.y, -p.x * s + p.z * c);
}

// =============================================
// SHARD DATA STRUCTURE
// =============================================

struct Shard {
    float id;
    vec2 center;
    vec3 normal;
    vec2 offset;
    float rot_z;
    float z_depth;
    float activation;
};

// =============================================
// SPIDER WEB GEOMETRY
// =============================================

float spiral_r(float theta, float ring, float num_spirals, float num_radials, float indent) {
    float base_r = 0.03;
    float max_r = 1.15;
    float spacing = (max_r - base_r) / num_spirals;
    float r = base_r + ring * spacing;
    float ang = num_radials * theta;
    float modulation = abs(sin(ang * 0.5)) * indent * (r + 0.1);
    return r + modulation;
}

// Get cell indices for spider web pattern
vec2 get_cell_indices(vec2 p, vec2 center, float num_radials, float num_spirals, float indent) {
    vec2 polar = to_polar(p, center);
    float r = polar.x;
    float theta = polar.y + PI;
    
    float radial_idx = floor(theta / (TAU / num_radials));
    
    float spiral_idx = 0.0;
    for (float ring = 0.0; ring < 32.0; ring += 1.0) {
        if (ring >= num_spirals) break;
        float ring_r = spiral_r(polar.y, ring, num_spirals, num_radials, indent);
        if (r > ring_r) spiral_idx = ring;
    }
    
    return vec2(radial_idx, spiral_idx);
}

// Calculate approximate cell center
vec2 get_cell_center(float radial_idx, float spiral_idx, float num_radials,
                     float num_spirals, float indent, vec2 center) {
    float theta_mid = (radial_idx + 0.5) * TAU / num_radials - PI;
    float r_inner = spiral_r(theta_mid, spiral_idx, num_spirals, num_radials, indent);
    float r_outer = spiral_r(theta_mid, spiral_idx + 1.0, num_spirals, num_radials, indent);
    float r_mid = (r_inner + r_outer) * 0.5;
    return center + vec2(cos(theta_mid), sin(theta_mid)) * r_mid;
}

// Web crack pattern
float web_cracks(vec2 uv, vec2 center, float num_radials, float num_spirals, float indent) {
    vec2 polar = to_polar(uv, center);
    float r = polar.x;
    float theta = polar.y + PI;
    
    float crack_width = 0.004;
    float crack = 0.0;
    
    // Radial cracks
    float sector = TAU / num_radials;
    float radial_angle = mod(theta, sector);
    float radial_dist = min(radial_angle, sector - radial_angle) * r;
    crack = max(crack, 1.0 - smoothstep(0.0, crack_width, radial_dist));
    
    // Spiral cracks
    float ang = num_radials * atan(uv.y - center.y, uv.x - center.x);
    float spiral_col = sin(r) + (r + 0.1) * abs(sin(ang * 0.5) * indent);
    float spiral_pattern = fract(spiral_col * num_spirals + ang * 0.0095);
    float spiral_crack = smoothstep(0.96, 0.98, spiral_pattern) + smoothstep(0.02, 0.0, spiral_pattern);
    crack = max(crack, spiral_crack);
    
    // Hub
    crack = max(crack, 1.0 - smoothstep(0.02, 0.025, r));
    
    // Limit to web area
    crack *= step(0.02, r) * step(r, 1.2);
    
    return crack;
}

// =============================================
// SHARD PHYSICS
// =============================================

Shard get_shard_physics(vec2 cell_idx, vec2 cell_center, vec2 web_center, float time_t, float num_spirals) {
    Shard s;
    s.id = cell_idx.x + cell_idx.y * 100.0;
    
    vec3 rnd = hash3(vec2(s.id, s.id * 0.123));
    vec3 rnd2 = hash3(vec2(s.id * 1.7, s.id * 0.456));
    
    // Activation: outer rings shatter slightly later (ripple effect)
    float ring_delay = cell_idx.y / num_spirals * 0.3;
    s.activation = smoothstep(ring_delay, ring_delay + 0.4, time_t);
    
    // Direction away from impact center
    vec2 dir = cell_center - web_center;
    float dist = length(dir);
    if (dist < 0.001) dir = vec2(0.0, 1.0);
    else dir = normalize(dir);
    
    // Add some randomness to direction
    float angle_offset = (rnd.x - 0.5) * 0.8;
    float c = cos(angle_offset), ss = sin(angle_offset);
    dir = vec2(dir.x * c - dir.y * ss, dir.x * ss + dir.y * c);
    
    // Velocity based on distance from center (closer = faster initially)
    float speed = (1.2 - dist * 0.5) * (rnd.y * 0.5 + 0.75);
    speed *= flame_speed * 0.0015;
    
    // Movement with easing
    float ease = s.activation * s.activation;
    s.offset = dir * ease * speed * 3.0;
    
    // Gravity droop
    s.offset.y -= ease * ease * 0.15 * (1.0 + rnd.z * 0.5);
    
    // Z-axis tumble rotation
    float rot_speed = (rnd2.x - 0.5) * 8.0;
    s.rot_z = ease * rot_speed;
    
    // 3D Tilt for normal (creates the angle change effect)
    float tilt_x = (rnd.x - 0.5) * ease * 2.5;
    float tilt_y = (rnd.y - 0.5) * ease * 2.5;
    
    // Early wobble before full shatter
    float wobble = sin(time_t * 20.0 + s.id) * 0.1 * (1.0 - ease);
    tilt_x += wobble;
    tilt_y += wobble * 0.7;
    
    vec3 n = vec3(0.0, 0.0, 1.0);
    n = rotate_x(n, tilt_x);
    n = rotate_y(n, tilt_y);
    s.normal = normalize(n);
    
    // Depth for shadows and layering
    s.z_depth = ease * (rnd2.z * 0.5 + 0.25);
    
    s.center = cell_center;
    
    return s;
}

// =============================================
// ENVIRONMENT & LIGHTING
// =============================================

vec3 get_env_map(vec3 dir) {
    float y = dir.y * 0.5 + 0.5;
    
    // Sky gradient
    vec3 sky = mix(vec3(0.5, 0.6, 0.75), vec3(0.2, 0.35, 0.7), pow(y, 1.5));
    vec3 horizon = vec3(0.85, 0.88, 0.92);
    vec3 ground = vec3(0.15, 0.12, 0.1);
    
    vec3 env;
    if (y > 0.5) {
        env = mix(horizon, sky, (y - 0.5) * 2.0);
    } else {
        env = mix(ground, horizon, y * 2.0);
    }
    
    // Studio lights
    vec3 light1 = normalize(vec3(1.0, 1.0, 0.8));
    vec3 light2 = normalize(vec3(-0.8, 0.5, 0.6));
    vec3 light3 = normalize(vec3(0.0, -0.3, 1.0));
    
    float spec1 = pow(max(0.0, dot(dir, light1)), 32.0);
    float spec2 = pow(max(0.0, dot(dir, light2)), 24.0) * 0.5;
    float spec3 = pow(max(0.0, dot(dir, light3)), 16.0) * 0.3;
    
    env += vec3(1.0, 0.98, 0.95) * spec1;
    env += vec3(0.9, 0.95, 1.0) * spec2;
    env += vec3(1.0) * spec3;
    
    return env;
}

float fresnel_schlick(float cos_theta, float f0) {
    return f0 + (1.0 - f0) * pow(clamp(1.0 - cos_theta, 0.0, 1.0), 5.0);
}

// =============================================
// MAIN
// =============================================

void main()
{
    // Early exit for out of bounds
    if (uvpos.x < 0.0 || uvpos.x > 1.0 || uvpos.y < 0.0 || uvpos.y > 1.0) {
        gl_FragColor = vec4(0.0);
        return;
    }
    
    // Aspect ratio correction
    float aspect = size.x / size.y;
    vec2 p = (uvpos - 0.5);
    p.x *= aspect;
    
    // Animation timing
    float t = clamp(progress, 0.0, 1.0);
    float explode_t = pow(t, 1.8) * 2.5; // Eased explosion timing
    float fade_t = smoothstep(0.75, 1.0, t);
    
    // Web parameters
    vec2 web_center = vec2(0.0, 0.0);
    float num_radials = 24.0 + flame_width * 0.08;
    float num_spirals = 18.0 + flame_height * 0.06;
    float indent = 0.008;
    
// =========================================
// IMPACT SHOCKWAVE
// =========================================
float shockwave_speed = 2.5;
float shockwave_radius = t * shockwave_speed;
float shockwave_width = 0.15;
float shockwave_strength = 0.08;

float dist_from_center = length(p - web_center);
float shockwave_ring = abs(dist_from_center - shockwave_radius);
float shockwave_factor = 1.0 - smoothstep(0.0, shockwave_width, shockwave_ring);

// Fade out shockwave over time
shockwave_factor *= (1.0 - smoothstep(0.3, 0.7, t));

// Distort coordinates outward from center
vec2 shockwave_dir = normalize(p - web_center + 0.0001);
vec2 shockwave_distort = shockwave_dir * shockwave_factor * shockwave_strength;

// Apply distortion to p for the rest of the effect
p += shockwave_distort;    


    // Get cell for current position
    vec2 cell_idx = get_cell_indices(p, web_center, num_radials, num_spirals, indent);
    vec2 cell_center = get_cell_center(cell_idx.x, cell_idx.y, num_radials, num_spirals, indent, web_center);
    
    // Check if in valid web area
    vec2 polar = to_polar(p, web_center);
    float in_web = step(0.02, polar.x) * step(polar.x, 1.2);
    
    // Get physics for this shard
    Shard shard = get_shard_physics(cell_idx, cell_center, web_center, explode_t, num_spirals);
    
    // =========================================
    // INVERSE TRANSFORM to find source texture
    // =========================================
    
    vec2 local_p = p;
    
    // Inverse rotation
    float c = cos(-shard.rot_z);
    float s = sin(-shard.rot_z);
    local_p = vec2(local_p.x * c - local_p.y * s, local_p.x * s + local_p.y * c);
    
    // Inverse translation
    local_p -= shard.offset;
    
    // Convert back to texture coordinates
    vec2 tex_uv = local_p;
    tex_uv.x /= aspect;
    tex_uv += 0.5;
    
    // Texture bounds check
    float tex_valid = step(0.0, tex_uv.x) * step(tex_uv.x, 1.0) * 
                      step(0.0, tex_uv.y) * step(tex_uv.y, 1.0);
    
    // Check if we crossed into a different cell (for crack borders)
    vec2 check_p = local_p;
    vec2 check_idx = get_cell_indices(check_p, web_center, num_radials, num_spirals, indent);
    float same_cell = 1.0 - step(0.5, length(check_idx - cell_idx));
    
    // Get crack pattern at source location
    float cracks = web_cracks(check_p, web_center, num_radials, num_spirals, indent);
    
    // =========================================
    // RENDERING
    // =========================================
    
    vec4 final_color = vec4(0.0);
    
    // View vector
    vec3 view_vec = vec3(0.0, 0.0, 1.0);
    float NdotV = max(0.0, dot(shard.normal, view_vec));
    
    // Combined mask
    float shard_mask = tex_valid * in_web * same_cell;
    
    if (shard_mask > 0.5) {
        // Surface normal with micro perturbation
        vec2 noise_uv = tex_uv * 80.0;
        vec3 micro_normal = vec3(
            (hash(noise_uv) - 0.5) * 0.08,
            (hash(noise_uv + 100.0) - 0.5) * 0.08,
            0.0
        );
        vec3 surface_normal = normalize(shard.normal + micro_normal);
        
        // Refraction offset based on normal tilt
        vec2 refract_offset = surface_normal.xy * SHARD_THICKNESS;
        
        // Chromatic aberration (RGB split)
// Add shockwave distortion to texture sampling
vec2 tex_shockwave = shockwave_dir * shockwave_factor * 0.03;
tex_shockwave.x /= aspect;

vec2 uv_r = clamp(tex_uv - refract_offset * (1.0 + DISPERSION) + tex_shockwave, 0.0, 1.0);
vec2 uv_g = clamp(tex_uv - refract_offset + tex_shockwave, 0.0, 1.0);
vec2 uv_b = clamp(tex_uv - refract_offset * (1.0 - DISPERSION) + tex_shockwave, 0.0, 1.0);
        
        vec3 glass_color = vec3(
            get_pixel(uv_r).r,
            get_pixel(uv_g).g,
            get_pixel(uv_b).b
        );
        
        // Edge frosting (glass appears opaque at steep angles)
        float edge_factor = 1.0 - NdotV;
        edge_factor = smoothstep(0.5, 0.95, edge_factor);
        
        vec3 frost_color = mix(vec3(0.85, 0.9, 0.92), flame_color.rgb * 0.5, 0.3);
        frost_color *= (0.85 + 0.3 * hash(tex_uv * 60.0));
        
        glass_color = mix(glass_color, frost_color, edge_factor);
        
        // Glass tint (subtle color from thickness)
        vec3 glass_tint = mix(vec3(1.0), flame_color.rgb, 0.08);
   //     glass_color *= glass_tint;
        
        // Fresnel reflection
        float f0 = pow((IOR - 1.0) / (IOR + 1.0), 2.0);
        float fresnel = fresnel_schlick(NdotV, f0);
        
        // Increase fresnel when tilted (shows more reflection)
        fresnel += edge_factor * 0.3;
        fresnel = clamp(fresnel, 0.0, 0.8);
        
        // Environment reflection
        vec3 reflect_vec = reflect(-view_vec, surface_normal);
        vec3 env_color = get_env_map(reflect_vec);
        
        // Combine refraction and reflection
        final_color.rgb = mix(glass_color, env_color, fresnel);
        
// Shine fade: fade in at start, fade out at end
float shine_fade = smoothstep(0.0, 0.2, t) * (1.0 - smoothstep(0.7, 1.0, t));

// Specular highlights
vec3 light_dir = normalize(vec3(0.5, 0.8, 0.6));
vec3 half_vec = normalize(light_dir + view_vec);
float spec = pow(max(0.0, dot(surface_normal, half_vec)), 48.0);
final_color.rgb += vec3(1.0, 0.98, 0.95) * spec * 3.0 * shine_fade;

// Secondary light
vec3 light2_dir = normalize(vec3(-0.7, 0.4, 0.5));
vec3 half_vec2 = normalize(light2_dir + view_vec);
float spec2 = pow(max(0.0, dot(surface_normal, half_vec2)), 32.0);
final_color.rgb += vec3(0.9, 0.95, 1.0) * spec2 * 1.5 * shine_fade;

// Edge highlight (light catching shard edges)
float edge_highlight = edge_factor * pow(max(0.0, dot(surface_normal.xy, normalize(vec2(0.6, 0.8)))), 2.0);
final_color.rgb += vec3(1.0) * edge_highlight * 1.2 * shine_fade;
        
        // Crack rendering
        float crack_glow = cracks * (0.5 + shard.activation * 0.5);
        vec3 crack_dark = vec3(0.05, 0.05, 0.08);
        vec3 crack_edge = flame_color.rgb * 0.8 + vec3(0.2);
        
        // Dark crack line with bright edge
      //  final_color.rgb = mix(final_color.rgb, crack_dark, cracks * 0.7);
     //   final_color.rgb += crack_edge * cracks * 0.3 * shard.activation;
        
        // Sparkle
        float sparkle = sin(shard.id * 17.0 + t * 15.0) * sin(polar.x * 200.0);
        sparkle = pow(max(0.0, sparkle), 16.0) * 0.4;
      //  final_color.rgb += vec3(1.0) * sparkle;
        
        final_color.a = 1.0;
        
    } else {
        // Outside shard - fully transparent
        final_color = vec4(0.0, 0.0, 0.0, 0.0);
    }
    
    // Fade out at end
    final_color.a *= (1.0 - fade_t);
    
    // Soft edges on screen bounds
 //   float edge_soft = smoothstep(0.0, 0.02, uvpos.x) * smoothstep(0.0, 0.02, 1.0 - uvpos.x) *
    //                  smoothstep(0.0, 0.02, uvpos.y) * smoothstep(0.0, 0.02, 1.0 - uvpos.y);
    
 //   final_color.a *= edge_soft;
    
    gl_FragColor = clamp(final_color, 0.0, 1.0);
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
