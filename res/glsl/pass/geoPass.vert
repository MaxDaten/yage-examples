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
out vec3 VertexPos;
out mat3 TBN;

void main()
{
    mat4 MVMatrix   = ViewMatrix * ModelMatrix;
    mat4 MVPMatrix  = VPMatrix * ModelMatrix;
    
    // flip vertical (t or v) because opengl's first row in an image is bottom left (instead of top left)
    // tangents are respected in the frag-shaders for NormalY calculation (cross arguments are flipped)
    VertexUV                = vec2(vTexture.s, 1 - vTexture.t); // TODO : move to CPU
    vec3 tangentZ           = normalize( mat3(ViewMatrix) * NormalMatrix * vTangentZ.xyz );
    vec3 tangentX           = normalize( mat3(ViewMatrix) * NormalMatrix * vTangentX.xyz );
    vec3 tangentY           = cross(tangentX, tangentZ) * vTangentZ.w;
    VertexPos               = vec3(MVMatrix * vec4(vPosition, 1.0));
    TBN                     = transpose( mat3 ( tangentX, tangentY, tangentZ ) );

    gl_Position = MVPMatrix * vec4(vPosition, 1.0);
}
