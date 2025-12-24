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
uniform float window_seed;
// ============================================
// DALÍ MELTING CLOCK
// 3D surface lighting with proper normals
// ============================================
float hash(vec2 p) {
    return fract(sin(dot(p + window_seed, vec2(127.1, 311.7))) * 43758.5453);
}

// Calculate warp at any point - needed for derivatives
vec2 calculate_warp(vec2 pos, float zoom, float strength, float aspect) {
    vec2 warped = pos;
    
    // Droop calculation
    float bottom_edge = -1.5 * zoom;
    float droop_amount = strength * 0.6;
    float normalized_x = pos.x / (0.5 * aspect * zoom);
    float parabola = max(0.0, 1.0 - normalized_x * normalized_x);
    float droop_at_x = droop_amount * parabola;
    float new_bottom = bottom_edge - droop_at_x;
    
    if (pos.y < bottom_edge && pos.y > new_bottom && droop_at_x > 0.001) {
        float droop_progress = (bottom_edge - pos.y) / droop_at_x;
        warped.y = bottom_edge + droop_progress * 0.1 * zoom;
        float pinch = droop_progress * 0.4 * parabola;
        warped.x *= 1.0 - pinch;
    }
    
    // Diagonal warps - seed affects positions
    vec2 center_offset = vec2(0.0);
    for (int i = 0; i < 20; i++) {
        float fi = float(i);
        float diag_pos = (-1.5 + fi * 0.2 + hash(vec2(fi, 4.0)) * 0.1) * zoom;
        float fold_strength = 0.3 + hash(vec2(fi, 5.0)) * 0.4;
        
        // Center offset
        float fold_zone_c = (0.0 - diag_pos) * 2.0;
        float fold_c = 1.0 / (1.0 + exp(-fold_zone_c * 3.0));
        float warp_c = fold_c * (1.0 - fold_c) * 4.0 * fold_strength;
        center_offset.x += warp_c * strength * 0.08;
        center_offset.y -= warp_c * strength * 0.08;
        
        // Actual warp
        float diag = pos.x / aspect + pos.y;
        float fold_zone = (diag - diag_pos) * 2.0;
        float fold = 1.0 / (1.0 + exp(-fold_zone * 3.0));
        float warp_amt = fold * (1.0 - fold) * 4.0 * fold_strength;
        warped.x += warp_amt * strength * 0.08;
        warped.y -= warp_amt * strength * 0.08;
    }
    
    warped -= center_offset;
    return warped;
}

// Calculate surface height (Z) based on warp displacement
float calculate_height(vec2 pos, float zoom, float strength, float aspect) {
    float height = 0.0;
    
    // Droop creates downward curve
    float bottom_edge = -1.5 * zoom;
    float droop_amount = strength * 0.6;
    float normalized_x = pos.x / (0.5 * aspect * zoom);
    float parabola = max(0.0, 1.0 - normalized_x * normalized_x);
    float droop_at_x = droop_amount * parabola;
    float new_bottom = bottom_edge - droop_at_x;
    
    if (pos.y < bottom_edge && pos.y > new_bottom && droop_at_x > 0.001) {
        float droop_progress = (bottom_edge - pos.y) / droop_at_x;
        height = -droop_progress * droop_progress * 0.3 * strength;
    }
    
    // Diagonal folds create ridges and valleys
    for (int i = 0; i < 20; i++) {
        float fi = float(i);
        float diag_pos = (-1.5 + fi * 0.2 + hash(vec2(fi, 4.0)) * 0.1) * zoom;
        float fold_strength = 0.3 + hash(vec2(fi, 5.0)) * 0.4;
        
        float diag = pos.x / aspect + pos.y;
        float fold_zone = (diag - diag_pos) * 2.0;
        float fold = 1.0 / (1.0 + exp(-fold_zone * 3.0));
        
        float ridge = fold * (1.0 - fold) * 4.0;
        height += ridge * fold_strength * strength * 0.015;
    }
    
    return height;
}

void main()
{
    float aspect = size.x / size.y;
    vec2 p = (uvpos - 0.5);
    p.x *= aspect;
    
    float t = clamp(progress, 0.0, 1.0);
    float t2 = t * t;
    
    // Effect fade in
    float effect_fade = smoothstep(0.0, 0.15, t);
    
    // Lighting intensity ramps up during animation - subtle at start, pronounced by 30%
    float light_intensity = smoothstep(0.0, 0.3, t);
    
    // Zoom out
    float zoom = 1.0 + t * 1.0 * effect_fade;
    vec2 zoomed_p = p * zoom;
    
    // Warp strength
    float strength = t2 * (60.0 + flame_speed * 0.04) * effect_fade;
    
    // Check if below droop cutoff
    float bottom_edge = -1.5 * zoom;
    float droop_amount = strength * (0.6 + flame_height * 0.004);
    float normalized_x = zoomed_p.x / (0.5 * aspect * zoom);
    float parabola = max(0.0, 1.0 - normalized_x * normalized_x);
    float droop_at_x = droop_amount * parabola;
    float new_bottom = bottom_edge - droop_at_x;
    
    if (zoomed_p.y <= new_bottom) {
        gl_FragColor = vec4(0.0, 0.0, 0.0, 0.0);
        return;
    }
    
    // Calculate warped position
    vec2 warped = calculate_warp(zoomed_p, zoom, strength, aspect);
    
    // Calculate surface normal using partial derivatives
    float eps = 0.005 * zoom;
    
    float h_center = calculate_height(zoomed_p, zoom, strength, aspect);
    float h_right = calculate_height(zoomed_p + vec2(eps, 0.0), zoom, strength, aspect);
    float h_left = calculate_height(zoomed_p - vec2(eps, 0.0), zoom, strength, aspect);
    float h_up = calculate_height(zoomed_p + vec2(0.0, eps), zoom, strength, aspect);
    float h_down = calculate_height(zoomed_p - vec2(0.0, eps), zoom, strength, aspect);
    
    // Partial derivatives
    float dzdx = (h_right - h_left) / (2.0 * eps);
    float dzdy = (h_up - h_down) / (2.0 * eps);
    
    // Surface normal from gradient
    vec3 normal = normalize(vec3(-dzdx, -dzdy, 1.0));
    
    // Warp derivatives for Jacobian
    vec2 warp_right = calculate_warp(zoomed_p + vec2(eps, 0.0), zoom, strength, aspect);
    vec2 warp_left = calculate_warp(zoomed_p - vec2(eps, 0.0), zoom, strength, aspect);
    vec2 warp_up = calculate_warp(zoomed_p + vec2(0.0, eps), zoom, strength, aspect);
    vec2 warp_down = calculate_warp(zoomed_p - vec2(0.0, eps), zoom, strength, aspect);
    
    float dwdx_x = (warp_right.x - warp_left.x) / (2.0 * eps);
    float dwdy_y = (warp_up.y - warp_down.y) / (2.0 * eps);
    float dwdx_y = (warp_right.y - warp_left.y) / (2.0 * eps);
    float dwdy_x = (warp_up.x - warp_down.x) / (2.0 * eps);
    
    // Tangent vectors
    vec3 tangent_u = normalize(vec3(1.0, 0.0, dzdx) + vec3(dwdx_x - 1.0, dwdx_y, 0.0) * 0.5);
    vec3 tangent_v = normalize(vec3(0.0, 1.0, dzdy) + vec3(dwdy_x, dwdy_y - 1.0, 0.0) * 0.5);
    
    // Cross product for surface normal
    vec3 surface_normal = normalize(cross(tangent_u, tangent_v));
    
    // Blend normals - more pronounced as animation progresses
    normal = normalize(mix(vec3(0.0, 0.0, 1.0), mix(normal, surface_normal, 0.7), light_intensity));
    
    if (normal.z < 0.0) normal.z = -normal.z;
    
    // Key light from upper left
    vec3 key_light_dir = normalize(vec3(0.5, 0.7, 0.9));
    vec3 key_light_color = vec3(1.0, 0.95, 0.85);
    float key_diffuse = max(0.0, dot(normal, key_light_dir));
    
    // Fill light from right
    vec3 fill_light_dir = normalize(vec3(-0.4, 0.3, 0.7));
    vec3 fill_light_color = vec3(0.7, 0.8, 1.0);
    float fill_diffuse = max(0.0, dot(normal, fill_light_dir)) * 0.3;
    
    // Specular
    vec3 view_dir = vec3(0.0, 0.0, 1.0);
    vec3 half_vec = normalize(key_light_dir + view_dir);
    float specular = pow(max(0.0, dot(normal, half_vec)), 40.0);
    
    // Fresnel rim
    float fresnel = pow(1.0 - max(0.0, dot(normal, view_dir)), 4.0);
    
    // Convert to texture coordinates
    vec2 tex_uv = warped;
    tex_uv.x /= aspect;
    tex_uv += 0.5;
    
    if (tex_uv.x < 0.0 || tex_uv.x > 1.0 || tex_uv.y < 0.0 || tex_uv.y > 1.0) {
        gl_FragColor = vec4(0.0, 0.0, 0.0, 0.0);
        return;
    }
    
    vec4 tex_color = get_pixel(tex_uv);
    float src_alpha = tex_color.a;
    
    if (src_alpha < 0.001) {
        gl_FragColor = vec4(0.0, 0.0, 0.0, 0.0);
        return;
    }
    
    vec3 color = tex_color.rgb / src_alpha;
    
    // Start with original color, blend in lighting as animation progresses
    vec3 lit_color = color;
    
    // Lighting contribution increases with light_intensity
    float ambient = mix(1.0, 1.0, light_intensity);
    vec3 shaded_color = color * ambient;
    
    // Add directional lighting
    shaded_color += color * key_light_color * key_diffuse * 0.5 * light_intensity;
    shaded_color += color * fill_light_color * fill_diffuse * light_intensity;
    
    // Specular and rim - fade in
    shaded_color += key_light_color * specular * 0.4 * light_intensity;
    shaded_color += vec3(0.6, 0.7, 1.0) * fresnel * 0.15 * light_intensity;
    
    // Ambient occlusion - fade in
    float ao = 1.0 - max(0.0, -h_center * 2.0);
    shaded_color *= mix(1.0, ao, light_intensity);
    
    // Blend from original to shaded
    lit_color = mix(color, shaded_color, light_intensity);
    
    // Warm Dalí tint - subtle
    lit_color = mix(lit_color, lit_color * vec3(1.03, 1.0, 0.95), t * 0.3);
    
    // Fade out at end
    float fade_out = 1.0 - smoothstep(0.85, 1.0, t);
    float final_alpha = src_alpha * fade_out;
    
    gl_FragColor = vec4(lit_color * final_alpha, final_alpha);
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
static uint32_t window_counter = 0;

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
    float window_seed;

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
                self->program.uniform1f("window_seed", self->window_seed);
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
        
        // Generate unique seed for this window
        window_counter++;
        this->window_seed = float(window_counter) * 17.31 + 
                           float(bbox.x) * 0.013 + 
                           float(bbox.y) * 0.017 +
                           float(bbox.width) * 0.011 +
                           float(bbox.height) * 0.019;
        
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
        auto padding = 1000;
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
