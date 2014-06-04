#version 410 core

uniform mat4 AlbedoTextureMatrix = mat4(1.0);
uniform mat4 NormalTextureMatrix = mat4(1.0);
uniform mat4 ViewMatrix          = mat4(1.0);
uniform mat4 VPMatrix            = mat4(1.0);
uniform mat4 ModelMatrix         = mat4(1.0);
uniform mat3 NormalMatrix        = mat3(1.0);

// naturally in model-space
in vec3 vPosition;
in vec2 vTexture;
in vec3 vTangentX;
in vec4 vTangentZ;

out vec2 AlbedoST;
out vec2 NormalST;
out vec3 VertexPos_View;
out mat3 TangentToView;

void main()
{
    mat4 ModelToView     = ViewMatrix * ModelMatrix;
    mat4 ModelToProj     = VPMatrix * ModelMatrix;
    
    // flip vertical (t or v) because opengl's first row in an image is bottom left (instead of top left)
    // tangents are respected in the frag-shaders for NormalY calculation (cross arguments are flipped)
    AlbedoST             = (AlbedoTextureMatrix * vec4(vTexture, 0.0, 1.0)).st;
    NormalST             = (NormalTextureMatrix * vec4(vTexture, 0.0, 1.0)).st;
    
    vec3 tangentZ        = normalize( vTangentZ.xyz );
    vec3 tangentX        = normalize( vTangentX.xyz );
    vec3 tangentY        = normalize( cross( tangentZ, tangentX ) * vTangentZ.w );
    TangentToView        = mat3(ModelToView) * NormalMatrix * mat3( tangentX, tangentY, tangentZ );
    
    VertexPos_View  = vec3( ModelToView * vec4(vPosition, 1.0) );
    gl_Position     = ModelToProj * vec4( vPosition, 1.0 );
}
