#version 410 core


in vec3 vposition;
in vec2 vtexture;

uniform mat4 ProjMatrix     = mat4(0.0);
uniform mat4 ModelMatrix    = mat4(0.0);

out vec2 VertexTex;
out vec4 VertexPos;

void main()
{
    
    VertexPos   = ProjMatrix * ModelMatrix * vec4(vposition, 1.0);
    
    VertexTex   = vtexture;

    gl_Position = VertexPos;
}
