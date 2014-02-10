#version 330 core


in vec4 in_vert_position;
in vec2 in_vert_texture;

uniform mat4 projection_matrix  = mat4(0.0);
uniform mat4 model_matrix       = mat4(0.0);

out vec2 vertex_uv;
out vec4 vertex_position;

void main()
{
    
    vertex_position = projection_matrix * model_matrix * in_vert_position;
    
    vertex_uv          = in_vert_texture;

    gl_Position = vertex_position;
}
