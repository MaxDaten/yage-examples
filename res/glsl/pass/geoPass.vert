#version 410 core

uniform mat4 ViewMatrix        = mat4(0.0);
uniform mat4 VPMatrix          = mat4(0.0);
uniform mat4 ModelMatrix       = mat4(0.0);
uniform mat3 NormalMatrix      = mat3(0.0);

in vec3 vposition;
in vec2 vtexture;
in vec3 vnormal;
in vec3 vtangent;

out vec2 VertexTex;
out vec3 VertexNormal;
out vec3 VertexTangent;
out vec3 VertexPos;

void main()
{
    mat4 MVMatrix   = ViewMatrix * ModelMatrix;
    mat4 MVPMatrix  = VPMatrix * ModelMatrix;
    
    VertexTex               = vec2( vtexture.s, abs(vtexture.t - 1.0) );
    VertexNormal            = normalize(mat3(ViewMatrix) * NormalMatrix * vnormal);
    VertexTangent           = normalize(mat3(ViewMatrix) * NormalMatrix * vtangent);
    VertexPos               = vec3(MVMatrix * vec4(vposition, 1.0));

    gl_Position = MVPMatrix * vec4(vposition, 1.0);
}
