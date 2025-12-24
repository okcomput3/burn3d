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
// DALÍ MELTING CLOCK
// Extreme multiple draping folds
// ============================================

float hash(vec2 p) {
    return fract(sin(dot(p, vec2(127.1, 311.7))) * 43758.5453);
}

void main()
{
    float aspect = size.x / size.y;
    vec2 p = (uvpos - 0.5);
    p.x *= aspect;
    
    float t = clamp(progress, 0.0, 1.0);
    float t2 = t * t;
    
    // Zoom out massively
    float zoom = 1.0 + t * 30.0;
    vec2 zoomed_p = p * zoom;
    
    // Extreme warp strength
    float strength = t2 * (40.0 + flame_speed * 0.04);
    
    vec2 warped = zoomed_p;
    
    float total_droop = 0.0;
    float total_bulge = 0.0;
    
    // 40 horizontal folds
    for (int i = 0; i < 40; i++) {
        float fi = float(i);
        float fold_y = -2.0 + fi * 0.2 + hash(vec2(fi, 0.0)) * 0.15;
        fold_y *= zoom;
        
        float fold_strength = 0.4 + hash(vec2(fi, 1.0)) * 0.6;
        float fold_zone = (zoomed_p.y - fold_y) * 3.0;
        float fold = 1.0 / (1.0 + exp(-fold_zone * 4.0));
        
        float droop = fold * strength * 0.15 * fold_strength;
        total_droop += droop;
        
        float bulge = fold * (1.0 - fold) * 4.0;
        total_bulge += bulge * fold_strength;
    }
    
    // 30 vertical smudges
    for (int i = 0; i < 30; i++) {
        float fi = float(i);
        float fold_x = -2.0 + fi * 0.2 + hash(vec2(fi, 2.0)) * 0.15;
        fold_x *= zoom * aspect;
        
        float fold_strength = 0.3 + hash(vec2(fi, 3.0)) * 0.4;
        float fold_zone = (zoomed_p.x - fold_x) * 2.5;
        float fold = 1.0 / (1.0 + exp(-fold_zone * 3.5));
        
        float smear = fold * (1.0 - fold) * 4.0;
        warped.x += smear * strength * 0.12 * fold_strength * sign(zoomed_p.x - fold_x);
        
        total_bulge += smear * fold_strength * 0.4;
    }
    
    // 20 diagonal warps
    for (int i = 0; i < 20; i++) {
        float fi = float(i);
        float diag_pos = -1.5 + fi * 0.2 + hash(vec2(fi, 4.0)) * 0.1;
        diag_pos *= zoom;
        
        float fold_strength = 0.3 + hash(vec2(fi, 5.0)) * 0.4;
        float diag = zoomed_p.x / aspect + zoomed_p.y;
        float fold_zone = (diag - diag_pos) * 2.0;
        float fold = 1.0 / (1.0 + exp(-fold_zone * 3.0));
        
        float warp_amt = fold * (1.0 - fold) * 4.0 * fold_strength;
        warped.x += warp_amt * strength * 0.08;
        warped.y -= warp_amt * strength * 0.08;
        
        total_bulge += warp_amt * 0.3;
    }
    
    warped.y -= total_droop;
    warped.x *= 1.0 + total_bulge * strength * 0.01;
    
    // Backward tilt
    float top_zone = smoothstep(-1.0 * zoom, 1.0 * zoom, zoomed_p.y);
    warped.y += (1.0 - top_zone) * strength * 0.15;
    
    // Lighting gradient
    float h = 0.02;
    float droop_dy = 0.0;
    
    for (int i = 0; i < 10; i++) {
        float fi = float(i);
        float fold_y = (-2.0 + fi * 0.5 + hash(vec2(fi, 0.0)) * 0.15) * zoom;
        float fold_strength = 0.4 + hash(vec2(fi, 1.0)) * 0.6;
        
        float fold_here = 1.0 / (1.0 + exp(-((zoomed_p.y - fold_y) * 3.0) * 4.0));
        float fold_next = 1.0 / (1.0 + exp(-(((zoomed_p.y + h) - fold_y) * 3.0) * 4.0));
        
        droop_dy += (fold_next - fold_here) * strength * 0.15 * fold_strength;
    }
    
    vec3 normal = normalize(vec3(0.0, -droop_dy * 3.0, 1.0));
    
    vec3 light_dir = normalize(vec3(0.3, 0.6, 0.7));
    float diffuse = max(0.0, dot(normal, light_dir));
    
    vec3 view_dir = vec3(0.0, 0.0, 1.0);
    vec3 half_vec = normalize(light_dir + view_dir);
    float specular = pow(max(0.0, dot(normal, half_vec)), 15.0);
    
    vec2 tex_uv = warped;
    tex_uv.x /= aspect;
    tex_uv += 0.5;
    
    float in_bounds = step(0.0, tex_uv.x) * step(tex_uv.x, 1.0) * 
                      step(0.0, tex_uv.y) * step(tex_uv.y, 1.0);
    
    if (in_bounds < 0.5) {
        gl_FragColor = vec4(0.0, 0.0, 0.0, 0.0);
        return;
    }
    
    // Sample the texture
    vec4 tex_color = get_pixel(tex_uv);
    float src_alpha = tex_color.a;
    
    if (src_alpha < 0.001) {
        gl_FragColor = vec4(0.0, 0.0, 0.0, 0.0);
        return;
    }
    
    // Unpremultiply
    vec3 color = tex_color.rgb / src_alpha;
    
    // Subtle lighting - mostly preserves original brightness
    float light_amount = t * 0.15; // very subtle, increases with progress
    
    float lighting = 1.0 + (diffuse - 0.5) * light_amount;
    lighting += specular * light_amount * 0.3 * min(total_bulge * 0.1, 1.0);
    
    vec3 lit_color = color * lighting;
    
    // Final alpha
    float final_alpha = src_alpha * (1.0 - smoothstep(0.94, 1.0, t));
    
    // Premultiply for output
    gl_FragColor = vec4(lit_color * final_alpha, final_alpha);
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

// ============================================
// DALÍ MELTING CLOCK
// Extreme multiple draping folds
// ============================================

float hash(vec2 p) {
    return fract(sin(dot(p, vec2(127.1, 311.7))) * 43758.5453);
}

void main()
{
    float aspect = size.x / size.y;
    vec2 p = (uvpos - 0.5);
    p.x *= aspect;
    
    float t = clamp(progress, 0.0, 1.0);
    float t2 = t * t;
    
    // Zoom out massively
    float zoom = 1.0 + t * 30.0;
    vec2 zoomed_p = p * zoom;
    
    // Extreme warp strength
    float strength = t2 * (40.0 + flame_speed * 0.04);
    
    vec2 warped = zoomed_p;
    
    float total_droop = 0.0;
    float total_bulge = 0.0;
    
    // 40 horizontal folds
    for (int i = 0; i < 40; i++) {
        float fi = float(i);
        float fold_y = -2.0 + fi * 0.2 + hash(vec2(fi, 0.0)) * 0.15;
        fold_y *= zoom;
        
        float fold_strength = 0.4 + hash(vec2(fi, 1.0)) * 0.6;
        float fold_zone = (zoomed_p.y - fold_y) * 3.0;
        float fold = 1.0 / (1.0 + exp(-fold_zone * 4.0));
        
        float droop = fold * strength * 0.15 * fold_strength;
        total_droop += droop;
        
        float bulge = fold * (1.0 - fold) * 4.0;
        total_bulge += bulge * fold_strength;
    }
    
    // 30 vertical smudges
    for (int i = 0; i < 30; i++) {
        float fi = float(i);
        float fold_x = -2.0 + fi * 0.2 + hash(vec2(fi, 2.0)) * 0.15;
        fold_x *= zoom * aspect;
        
        float fold_strength = 0.3 + hash(vec2(fi, 3.0)) * 0.4;
        float fold_zone = (zoomed_p.x - fold_x) * 2.5;
        float fold = 1.0 / (1.0 + exp(-fold_zone * 3.5));
        
        float smear = fold * (1.0 - fold) * 4.0;
        warped.x += smear * strength * 0.12 * fold_strength * sign(zoomed_p.x - fold_x);
        
        total_bulge += smear * fold_strength * 0.4;
    }
    
    // 20 diagonal warps
    for (int i = 0; i < 20; i++) {
        float fi = float(i);
        float diag_pos = -1.5 + fi * 0.2 + hash(vec2(fi, 4.0)) * 0.1;
        diag_pos *= zoom;
        
        float fold_strength = 0.3 + hash(vec2(fi, 5.0)) * 0.4;
        float diag = zoomed_p.x / aspect + zoomed_p.y;
        float fold_zone = (diag - diag_pos) * 2.0;
        float fold = 1.0 / (1.0 + exp(-fold_zone * 3.0));
        
        float warp_amt = fold * (1.0 - fold) * 4.0 * fold_strength;
        warped.x += warp_amt * strength * 0.08;
        warped.y -= warp_amt * strength * 0.08;
        
        total_bulge += warp_amt * 0.3;
    }
    
    warped.y -= total_droop;
    warped.x *= 1.0 + total_bulge * strength * 0.01;
    
    // Backward tilt
    float top_zone = smoothstep(-1.0 * zoom, 1.0 * zoom, zoomed_p.y);
    warped.y += (1.0 - top_zone) * strength * 0.15;
    
    // Lighting gradient
    float h = 0.02;
    float droop_dy = 0.0;
    
    for (int i = 0; i < 10; i++) {
        float fi = float(i);
        float fold_y = (-2.0 + fi * 0.5 + hash(vec2(fi, 0.0)) * 0.15) * zoom;
        float fold_strength = 0.4 + hash(vec2(fi, 1.0)) * 0.6;
        
        float fold_here = 1.0 / (1.0 + exp(-((zoomed_p.y - fold_y) * 3.0) * 4.0));
        float fold_next = 1.0 / (1.0 + exp(-(((zoomed_p.y + h) - fold_y) * 3.0) * 4.0));
        
        droop_dy += (fold_next - fold_here) * strength * 0.15 * fold_strength;
    }
    
    vec3 normal = normalize(vec3(0.0, -droop_dy * 3.0, 1.0));
    
    vec3 light_dir = normalize(vec3(0.3, 0.6, 0.7));
    float diffuse = max(0.0, dot(normal, light_dir));
    
    vec3 view_dir = vec3(0.0, 0.0, 1.0);
    vec3 half_vec = normalize(light_dir + view_dir);
    float specular = pow(max(0.0, dot(normal, half_vec)), 15.0);
    
    vec2 tex_uv = warped;
    tex_uv.x /= aspect;
    tex_uv += 0.5;
    
    float in_bounds = step(0.0, tex_uv.x) * step(tex_uv.x, 1.0) * 
                      step(0.0, tex_uv.y) * step(tex_uv.y, 1.0);
    
    if (in_bounds < 0.5) {
        gl_FragColor = vec4(0.0);
        return;
    }
    
    vec3 color = get_pixel(tex_uv).rgb;
    
    float ambient = 0.65;
    float light_strength = min(strength * 0.2, 1.0);
    
    vec3 lit_color = color * (ambient + diffuse * (1.0 - ambient) * light_strength);
    lit_color += vec3(1.0, 0.99, 0.96) * specular * light_strength * 0.4 * min(total_bulge * 0.1, 1.0);
    
    float shadow = min(total_droop * 0.05, 0.35);
    lit_color *= 1.0 - shadow;
    
    float alpha = 1.0 - smoothstep(0.94, 1.0, t);
    
    gl_FragColor = vec4(lit_color, alpha);
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
