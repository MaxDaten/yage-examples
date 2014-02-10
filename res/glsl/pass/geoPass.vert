#version 330 core

uniform mat4 view_matrix        = mat4(0.0);
uniform mat4 projection_matrix  = mat4(0.0);
uniform mat4 vp_matrix          = mat4(0.0);
uniform mat4 model_matrix       = mat4(0.0);
uniform mat3 normal_matrix      = mat3(0.0);


in vec4 in_vert_position;
in vec3 in_vert_normal;
in vec2 in_vert_texture;
// in vec4 in_vert_color;

out vec3 vertex_normal;
out vec2 vertex_uv;
out vec3 vertex_position;

void main()
{
    mat4 modelview_matrix   = view_matrix * model_matrix;
    mat4 mvp_matrix         = vp_matrix * model_matrix;
    
    vertex_position = vec3( modelview_matrix * in_vert_position );
    vertex_normal   = normalize( normal_matrix * in_vert_normal );
    
    // interpolated_color = in_vert_color;
    vertex_uv          = in_vert_texture;

    gl_Position = mvp_matrix * in_vert_position;
}
