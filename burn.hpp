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
    
    // Optimized: Reduced iterations from 40 to 32 for performance
    for (int iter = 0; iter < 32; iter++)
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
    return tanh_approx(O / 2000.0);
}

// Simple hash for burn line noise
float hash1(float p) { return fract(sin(p * 127.1) * 43758.5453123); }
float hash2(vec2 p) { return fract(sin(dot(p, vec2(127.1, 311.7))) * 43758.5453123); }

void main()
{
    float width = size.x;
    float height = size.y;
    
    if (uvpos.x < 0.0 || uvpos.x > 1.0 || uvpos.y < 0.0 || uvpos.y > 1.0) {
        gl_FragColor = vec4(0.0);
        return;
    }
    
    float burn_progress = progress;
    if (direction == 1) burn_progress = 1.0 - burn_progress;
    
    float t = burn_progress * flame_speed * 10.0;
    
    // ============ WAVY BURN LINE ============
    // Fade wave amplitude at start and end so burn completes fully
    float wave_fade = smoothstep(0.0, 0.1, progress) * smoothstep(1.0, 0.85, progress);
    
    // Multiple wave frequencies for organic fire-creeping effect
    float wave_offset = 0.0;
    wave_offset += sin(uvpos.x * 12.0 + t * 0.4) * 0.025;           // Slow large waves
    wave_offset += sin(uvpos.x * 25.0 - t * 0.7) * 0.015;           // Medium counter-waves
    wave_offset += sin(uvpos.x * 45.0 + t * 1.2) * 0.008;           // Fast small ripples
    wave_offset += sin(uvpos.x * 8.0 + t * 0.2) * 0.035;            // Very slow broad undulation
    // Add pseudo-random variation per segment for irregular burn edge
    wave_offset += (hash2(vec2(floor(uvpos.x * 15.0), floor(t * 0.3))) - 0.5) * 0.02;
    
    // Apply fade so waves diminish at start and end
    wave_offset *= wave_fade;
    
    float effective_progress = progress + wave_offset;
    float dist_from_burn = uvpos.y - effective_progress;
    
    vec4 fire_color_accum = vec4(0.0);
    float a_3d = 0.0;

    // ============ 3D FIRE ============
    if (dist_from_burn >= -0.05 && dist_from_burn <= 0.5)
    {
        float fireY = (dist_from_burn - 0.05) * height * (0.2 + flame_height);
        float fire_width = max(width * 0.12, 5.0) * (1.5 + flame_width);
        float fire_res = max(height * 0.15, 40.0);
        vec3 iResolution = vec3(fire_width, fire_res, fire_res);
        float spacing = fire_width * 0.5;
        
        // Loop through layers manually to keep density
        // We use a simplified loop structure to reduce code bloat
        float offsets[6];
        offsets[0] = 0.0; offsets[1] = 1.5; offsets[2] = 3.0; 
        offsets[3] = 4.5; offsets[4] = 4.5; offsets[5] = 4.5;
        
        float shifts[6];
        shifts[0] = 0.0;  shifts[1] = 0.25; shifts[2] = 0.5;
        shifts[3] = 0.75; shifts[4] = 0.65; shifts[5] = 0.35;

        for(int i=0; i<6; i++) {
            for (float off = -spacing * 2.0 + spacing * shifts[i]; off <= width + spacing * 2.0; off += spacing) {
                float localX = uvpos.x * width - off - spacing * 0.5;
                if (abs(localX) < fire_width * (i < 2 ? 1.0 : 1.5)) {
                    fire_color_accum += render_fire(vec2(localX, fireY), iResolution, t + off * 0.01 + offsets[i]);
                }
            }
        }
        
        fire_color_accum = clamp(fire_color_accum, 0.0, 1.0);
        
        // Calculate Alpha
        a_3d = (fire_color_accum.r + fire_color_accum.g + fire_color_accum.b) / 3.0;
        a_3d = smoothstep(0.45, 0.9, a_3d);
        a_3d *= clamp(progress * 10.0, 0.0, 1.0);
        
        // === FADE LOGIC: SIDES AND TOP ONLY ===
        // Fade Left/Right
        a_3d *= smoothstep(0.0, 0.1, uvpos.x) * smoothstep(1.0, 0.9, uvpos.x);
        // Fade Top Only (Do not multiply by bottom fade)
        a_3d *= smoothstep(0.0, 0.1, uvpos.y);
    }
    
    // ============ BACKGROUND & DISTORTION ============
    // Optimized: Only one texture fetch is performed
    float distort_amount = a_3d * 0.02;
    vec2 distort_uv = uvpos + vec2(
        sin(uvpos.y * 30.0 + t * 5.5) * distort_amount,
        cos(uvpos.x * 30.0 + t * 4.0) * distort_amount
    );
    distort_uv = clamp(distort_uv, 0.0, 1.0);
    
    vec4 bg = get_pixel(distort_uv);
    if (distort_uv.y < effective_progress) bg = vec4(0.0);
    
    // ====== Charred edge ======
    float char_zone = smoothstep(0.0, 0.03, dist_from_burn) * smoothstep(0.06, 0.03, dist_from_burn);
    vec3 char_color = vec3(0.1, 0.05, 0.0);
    bg.rgb = mix(bg.rgb, char_color, char_zone * 0.7);

// ====== Blue fire zone ======
    float blue_height = 0.13;
    float blue_start = 0.02;
    float blue_zone = smoothstep(blue_start, blue_start + 0.02, dist_from_burn) 
                    * smoothstep(blue_start + blue_height, blue_start + blue_height * 0.5, dist_from_burn);
    
    float blue_flicker = 0.7 + 0.3 * sin(t * 12.0 + uvpos.x * 30.0) * sin(t * 8.0 + uvpos.x * 50.0);
    float blue_noise = sin(uvpos.x * 60.0 + t * 3.0) * 0.3 + sin(uvpos.x * 120.0 - t * 5.0) * 0.2;
    
    blue_zone *= blue_flicker * (0.8 + blue_noise * 0.4);
    
    // === FIX: FADE SIDES AND TOP (Matching the main fire) ===
    blue_zone *= smoothstep(0.0, 0.1, uvpos.x) * smoothstep(1.0, 0.9, uvpos.x); // Sides
    blue_zone *= smoothstep(0.0, 0.1, uvpos.y); // Top
    
    blue_zone = clamp(blue_zone, 0.0, 1.0);
    
    vec3 blue_color = mix(vec3(0.0, 0.2, 0.8), vec3(0.3, 0.5, 1.0), smoothstep(blue_start, blue_start + blue_height * 0.7, dist_from_burn));
    bg.rgb = mix(bg.rgb, blue_color, blue_zone * 0.85);

    // Composite 3D fire
    vec4 result = vec4(fire_color_accum.rgb, 1.0) * a_3d + bg * (1.0 - a_3d);
    
    // ============ BURN LINE ============
    float burn_size = 4.0;
    float wave = sin(uvpos.x * 40.0 + t * 2.0) * 0.003 + sin(uvpos.x * 80.0 - t * 3.0) * 0.002;
    float edge_noise = hash2(vec2(floor(uvpos.x * width * 0.5), floor(t * 2.0))) * 0.01;
    float adjusted_dist = dist_from_burn + wave + edge_noise;
    
    float ember_core = smoothstep(0.012 * burn_size, 0.0, abs(adjusted_dist)) * smoothstep(-0.005 * burn_size, 0.005 * burn_size, adjusted_dist);
    float inner_glow = smoothstep(0.025 * burn_size, 0.0, abs(adjusted_dist)) * smoothstep(-0.01 * burn_size, 0.01 * burn_size, adjusted_dist);
    float outer_glow = smoothstep(0.05 * burn_size, 0.0, abs(adjusted_dist)) * smoothstep(-0.02 * burn_size, 0.02 * burn_size, adjusted_dist);
    
    float ember_particle = 0.0;
    for (float i = 0.0; i < 5.0; i += 1.0) {
        float px = hash1(i + floor(t * 0.5)) * 0.9 + 0.05;
        float local_wave = (sin(px * 12.0 + t * 0.4) * 0.025 + sin(px * 25.0 - t * 0.7) * 0.015 + sin(px * 45.0 + t * 1.2) * 0.008 + sin(px * 8.0 + t * 0.2) * 0.035) * wave_fade;
        float py = progress + local_wave + sin(t * (2.0 + i) + i * 3.14159) * 0.015 * burn_size;
        float spark_dist = length(vec2((uvpos.x - px) * width / height, uvpos.y - py));
        ember_particle += smoothstep(0.02 * burn_size, 0.0, spark_dist) * (0.5 + 0.5 * sin(t * 10.0 + i * 5.0));
    }
    
    vec3 burn_line = vec3(0.8, 0.2, 0.0) * outer_glow + vec3(1.0, 0.6, 0.1) * inner_glow + vec3(1.0, 0.95, 0.7) * ember_core + vec3(1.0, 0.5, 0.0) * ember_particle;
    float burn_alpha = max(max(outer_glow, inner_glow), max(ember_core, ember_particle * 0.8));
    
    burn_alpha *= (0.9 + 0.2 * sin(t * 20.0 + uvpos.x * 50.0) * sin(t * 17.0)); // Flicker
    burn_alpha *= clamp(progress * 10.0, 0.0, 1.0);
    // Apply Sides fade to burn line too for consistency (optional, but looks better)
    burn_alpha *= smoothstep(0.0, 0.025, uvpos.x) * smoothstep(1.0, 0.975, uvpos.x);
    
    result.rgb += burn_line * burn_alpha;
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
