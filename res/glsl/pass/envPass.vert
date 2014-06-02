#version 410 core

uniform mat4 ViewMatrix        = mat4(0.0);
uniform mat4 VPMatrix          = mat4(0.0);
uniform mat4 ModelMatrix       = mat4(0.0);

in vec3 vPosition;

out vec3 VertexUV;

mat4 MVPMatrix = VPMatrix * ModelMatrix;
void main()
{
    mat4 MVPMatrix  = VPMatrix * ModelMatrix;
    VertexUV        = vPosition;
    gl_Position     = MVPMatrix * vec4( vPosition, 1.0 );
}
