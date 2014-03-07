#version 410 core


in vec3 vposition;
in vec2 vtexture;

uniform mat4 projection_matrix  = mat4(0.0);
uniform mat4 model_matrix       = mat4(0.0);

out vec2 VertexTex;
out vec4 VertexPos;

void main()
{
    
    VertexPos   = projection_matrix * model_matrix * vec4(vposition, 1.0);
    
    VertexTex   = vtexture;

    gl_Position = VertexPos;
}
