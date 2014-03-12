#version 410 core

uniform mat4 ViewMatrix        = mat4(0.0);
uniform mat4 VPMatrix          = mat4(0.0);
uniform mat4 ModelMatrix       = mat4(0.0);

in vec3 vposition;

out vec3 VertexTex;

mat4 MVPMatrix = VPMatrix * ModelMatrix;
void main()
{
    mat4 MVPMatrix  = VPMatrix * ModelMatrix;
    VertexTex       = vposition;
    gl_Position     = MVPMatrix * vec4( vposition, 1.0 );
}
