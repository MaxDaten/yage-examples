#version 410 core

uniform mat4 ViewMatrix        = mat4(0.0);
uniform mat4 VPMatrix          = mat4(0.0);
uniform mat4 ModelMatrix       = mat4(0.0);
uniform mat3 NormalMatrix      = mat3(0.0);

in vec3 vPosition;
in vec2 vTexture;
in vec4 vTangentX;
in vec4 vTangentZ;

out vec2 VertexUV;
out vec3 VertexNormal;
out vec3 VertexTangent;
out vec3 VertexPos;

void main()
{
    mat4 MVMatrix   = ViewMatrix * ModelMatrix;
    mat4 MVPMatrix  = VPMatrix * ModelMatrix;
    
    // flip vertical (t or v) because opengl's first row in an image is bottom left (instead of top left)
    // tangents are respected in the frag-shaders for NormalY calculation (cross arguments are flipped)
    VertexUV                = vec2(vTexture.s, 1 - vTexture.t); // TODO : move to CPU
    VertexNormal            = normalize(mat3(ViewMatrix) * NormalMatrix * vTangentZ.xyz);
    VertexTangent           = normalize(mat3(ViewMatrix) * NormalMatrix * (vTangentZ.w * vTangentX.xyz));
    VertexPos               = vec3(MVMatrix * vec4(vPosition, 1.0));

    gl_Position = MVPMatrix * vec4(vPosition, 1.0);
}
