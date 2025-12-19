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

// Simple hash for noise
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

// 2D smooth noise with C2 continuity (quintic interpolation)
float smooth_noise2D(vec2 p) {
    vec2 i = floor(p);
    vec2 f = fract(p);
    vec2 u = f * f * f * (f * (f * 6.0 - 15.0) + 10.0);
    
    float a = hash2(i + vec2(0.0, 0.0));
    float b = hash2(i + vec2(1.0, 0.0));
    float c = hash2(i + vec2(0.0, 1.0));
    float d = hash2(i + vec2(1.0, 1.0));
    
    return mix(mix(a, b, u.x), mix(c, d, u.x), u.y);
}

// Paper fiber density using FBM
float paper_fiber(vec2 uv, float t) {
    float fiber = 0.0;
    fiber += smooth_noise2D(uv * 15.0) * 0.5;
    fiber += smooth_noise2D(uv * 35.0 + t * 0.01) * 0.3;
    fiber += smooth_noise2D(uv * 70.0) * 0.2;
    return fiber;
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
    
    // Paper fiber at this pixel
    float fiber = paper_fiber(uvpos, t);
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
    // Burned area - transparent
    if (!inside_unburned) {
        gl_FragColor = vec4(0.0);
        return;
    }

    float distort_fade = smoothstep(0.0, 0.1, progress) * smoothstep(1.0, 0.85, progress);

    // === PAPER CURLING EFFECT ===
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
    
    // === HEAT SHIMMER ===
    float heat_zone = smoothstep(0.12, 0.0, dist_from_burn) * smoothstep(-0.02, 0.02, dist_from_burn);
    heat_zone *= distort_fade;
    
    total_distort += vec2(
        sin(uvpos.y * 50.0 + t * 8.0) * heat_zone * 0.008,
        cos(uvpos.x * 45.0 + t * 7.0) * heat_zone * 0.008
    );
    
    float rising_shimmer = sin(uvpos.x * 60.0 + t * 4.0 - uvpos.y * 30.0) * heat_zone * 0.006;
    rising_shimmer += sin(uvpos.x * 90.0 + t * 6.0 - uvpos.y * 45.0) * heat_zone * 0.004;
    total_distort.y += rising_shimmer;
    
    vec2 distort_uv = clamp(uvpos + total_distort, 0.0, 1.0);
    vec4 result = get_pixel(distort_uv);
    
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
        if (auto tr =
            }

            return running;
        }

        return false;
    }

    void reverse() override
    {
                view->get_transformed_node()->get_transformer<wf::burn::burn_transformer>(
                    burn_transformer_name))
        {
            tr->progression.reverse();
        }
    }
};
}
}
