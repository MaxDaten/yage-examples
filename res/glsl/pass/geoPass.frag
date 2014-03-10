#version 410 core


// geometric zFar plane (is located along the negative z axis for RHS)
uniform float ZFar;

uniform sampler2D AlbedoTexture;
uniform sampler2D NormalTexture;

in vec3 VertexPos;
in vec2 VertexTex;
in vec3 VertexNormal;
in vec3 VertexTangent;

// Red Green Blue Depth
layout (location = 0) out vec4 albedoOut;
layout (location = 1) out vec4 normalOut;
// layout (location = 2) out vec3 specularOut;
// layout (location = 3) out vec3 glossyOut;

vec3 bumpNormal (vec2 texCoord);

void main()
{

    albedoOut.rgb   = texture(AlbedoTexture, VertexTex).xyz;
    albedoOut.a     = VertexPos.z / ZFar;

    vec3 normal    = normalize(VertexNormal);
    vec3 tangent   = normalize(VertexTangent);
    vec3 bitangent = cross(normal, tangent);

    // needed to move the tangents from tangent space to object space
    mat3 tbn       = mat3(tangent, bitangent, normal);

    normalOut.rgb   = tbn * bumpNormal(VertexTex) * 0.5 + 0.5;
    normalOut.a     = 1;
}


vec3 bumpNormal(vec2 texCoord)
{
    vec3 bump;
    bump.xy = 2.0 * texture(NormalTexture, texCoord).gr - 1.0;
    bump.z  = sqrt(1.0 - bump.x * bump.x - bump.y * bump.y);
    // bump = 2.0 * texture(tex_normal, texCoord).rgb - 1.0;
    return normalize(bump);
}

