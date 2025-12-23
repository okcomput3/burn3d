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
// SALVADOR DALI MELTING + DISSOLUTION
// Warping effect with dissolving edge
// ============================================

float hash(vec2 p) {
    return fract(sin(dot(p, vec2(127.1, 311.7))) * 43758.5453);
}

float noise(vec2 p) {
    vec2 i = floor(p);
    vec2 f = fract(p);
    f = f * f * (3.0 - 2.0 * f);
    
    float a = hash(i);
    float b = hash(i + vec2(1.0, 0.0));
    float c = hash(i + vec2(0.0, 1.0));
    float d = hash(i + vec2(1.0, 1.0));
    
    return mix(mix(a, b, f.x), mix(c, d, f.x), f.y);
}

float fbm(vec2 p) {
    float v = 0.0;
    float a = 0.5;
    v += a * noise(p); p *= 2.0; a *= 0.5;
    v += a * noise(p); p *= 2.0; a *= 0.5;
    v += a * noise(p); p *= 2.0; a *= 0.5;
    v += a * noise(p);
    return v;
}

// Gradient of noise field
vec2 grad_noise(vec2 p, float t) {
    float h = 0.01;
    float scale = 1.5 + flame_width * 0.01;
    vec2 q = p * scale + t * 0.2;
    
    float r = fbm(q + vec2(h, 0.0));
    float l = fbm(q - vec2(h, 0.0));
    float u = fbm(q + vec2(0.0, h));
    float d = fbm(q - vec2(0.0, h));
    
    return vec2(r - l, u - d) / (2.0 * h);
}

// Curl gives smooth, swirling distortion
vec2 curl(vec2 p, float t) {
    vec2 g = grad_noise(p, t);
    return vec2(-g.y, g.x);
}

// Dissolution mask
float dissolve_mask(vec2 p, float t, float aspect) {
    // Normalize distances so they're equal regardless of aspect ratio
    // p.x is already multiplied by aspect, so divide it back for distance calc
    float dist_left = (p.x / aspect) + 0.5;
    float dist_right = 0.5 - (p.x / aspect);
    float dist_top = 0.5 - p.y;
    float dist_bottom = p.y + 0.5;
    
    // Use the minimum distance to nearest edge
    float dist = min(min(dist_left, dist_right), min(dist_top, dist_bottom));
    
    // Turbulent edge
    dist += fbm(p * 5.0 + t * 0.4) * 0.25;
    dist += fbm(p * 10.0 - t * 0.2) * 0.1;
    
    // Front goes past 0.5 to fully dissolve everything
    float front = t * 1.0;
    return smoothstep(front - 0.05, front + 0.05, dist);
}

void main()
{
    float aspect = size.x / size.y;
    vec2 p = (uvpos - 0.5);
    p.x *= aspect;
    
    float t = clamp(progress, 0.0, 1.0);
    float anim_t = t * 3.0;
    
    // Warp strength increases with progress
    float warp_amount = t * t * (2.3 + flame_speed * 0.001);
    
    // Multi-layer curl distortion for that melting look
    vec2 warp = vec2(0.0);
    
    // Large slow swirls
    warp += curl(p * 0.2, anim_t * 0.5) * 0.5;
    
    // Medium swirls
    warp += curl(p * 0.4, anim_t * 0.7) * 0.3;
    
    // Small detail swirls
    warp += curl(p * 0.8, anim_t) * 0.15;
    
    // Apply warp
    vec2 warped = p + warp * warp_amount;
    
    // Add some dripping/melting downward bias
    float drip = fbm(p * 3.0 + anim_t * 0.3) * 0.5 + 0.5;
    warped.y -= t * t * drip * 0.15 * (1.0 + flame_height * 0.005);
    
    // Convert back to texture coords
    vec2 tex_uv = warped;
    tex_uv.x /= aspect;
    tex_uv += 0.5;
    
    // Get dissolution
    float dissolve = dissolve_mask(p, t, aspect);
    
    // If fully dissolved, output transparent
    if (dissolve < 0.01) {
        gl_FragColor = vec4(0.0);
        return;
    }
    
    // Velocity for effects
    vec2 vel = curl(p, anim_t);
    
    float in_bounds = step(0.0, tex_uv.x) * step(tex_uv.x, 1.0) * 
                      step(0.0, tex_uv.y) * step(tex_uv.y, 1.0);
    
    vec4 final_color = vec4(0.0);
    
    if (in_bounds > 0.5 && dissolve > 0.01) {
        // Chromatic aberration from velocity
        float vel_mag = length(vel) * 0.06 * t;
        vec2 vel_dir = vel_mag > 0.0001 ? normalize(vel) : vec2(1.0, 0.0);
        vec2 offset = vel_dir * vel_mag / vec2(aspect, 1.0);
        
        vec3 color = vec3(
            get_pixel(clamp(tex_uv + offset, 0.0, 1.0)).r,
            get_pixel(tex_uv).g,
            get_pixel(clamp(tex_uv - offset, 0.0, 1.0)).b
        );
        
        // Edge glow
        float edge = smoothstep(0.25, 0.0, dissolve) * smoothstep(0.0, 0.08, dissolve);
        color += (flame_color.rgb * 1.5 + 0.3) * edge * 2.5;
        
        // Vortex glow
        float vort = length(curl(p, anim_t));
        color += flame_color.rgb * vort * 0.15 * t;
        
        final_color = vec4(color, dissolve);
    }
    
    // Particles in dissolved region
    if (dissolve < 0.4 && t > 0.1) {
        vec2 part_uv = p + curl(p * 1.5, anim_t) * t * 0.4;
        float parts = fbm(part_uv * 12.0 + anim_t * 0.8);
        parts = smoothstep(0.55, 0.75, parts);
        
        float part_alpha = parts * (0.4 - dissolve) * 1.5;
        vec3 part_col = flame_color.rgb * 0.8;
        
        final_color.rgb += part_col * part_alpha;
        final_color.a = max(final_color.a, part_alpha * 0.7);
    }
    
    // Fade out at end
    final_color.a *= 1.0 - smoothstep(0.9, 1.0, t);
    
    gl_FragColor = final_color;
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
