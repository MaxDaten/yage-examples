#version 410 core


// geometric zFar plane (is located along the negative z axis for RHS)
uniform float ZFar;

uniform sampler2D AlbedoTexture;
uniform sampler2D NormalTexture;
uniform int Textured;

uniform vec3 MaterialColor;
uniform float MaterialSpecular;

in vec3 VertexPos;
in vec2 VertexTex;
in vec3 VertexNormal;
in vec3 VertexTangent;

// Red Green Blue Depth
layout (location = 0) out vec4 albedoOut;
layout (location = 1) out vec4 normalOut;
// layout (location = 2) out vec3 specularOut;
// layout (location = 3) out vec3 glossyOut;

vec3 BumpNormal (vec2 texCoord);
vec3 EncodeNormal (vec3 normal3d);

void main()
{
    if (Textured == 1)
        albedoOut.rgb   = texture(AlbedoTexture, VertexTex).xyz;
    else
        albedoOut.rgb   = MaterialColor.rgb;

    albedoOut.a     = VertexPos.z / ZFar;

    vec3 normal    = normalize(VertexNormal);
    vec3 tangent   = normalize(VertexTangent);
    vec3 bitangent = cross(normal, tangent);

    // needed to move the tangents from tangent space to object space
    mat3 tbn       = mat3(tangent, bitangent, normal);

    normalOut.rgb   = EncodeNormal(tbn * BumpNormal(VertexTex));
    normalOut.a     = 1;
    MaterialSpecular;
}

vec3 EncodeNormal (vec3 normal3d)
{
    return normal3d.xyz * 0.5 + 0.5;
}


vec3 BumpNormal(vec2 texCoord)
{
    vec2 normalColor = vec2(0.5, 0.5);
    if (Textured == 1)
        normalColor = texture(NormalTexture, texCoord).rg;
    
    vec3 bump;
    bump.xy = 2.0 * normalColor - 1.0;
    bump.z  = sqrt(1.0 - bump.x * bump.x - bump.y * bump.y);
    // bump = 2.0 * texture(tex_normal, texCoord).rgb - 1.0;
    return normalize(bump);
}

