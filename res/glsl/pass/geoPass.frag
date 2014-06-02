#version 410 core


// geometric zFar plane (is located along the negative z axis for RHS)
uniform float ZFar;

uniform sampler2D AlbedoTexture;
uniform sampler2D NormalTexture;

uniform vec4 AlbedoColor;
uniform vec4 NormalColor;
uniform float Specular = 1;

in vec3 VertexPos;
in vec2 VertexUV;
in vec3 VertexNormal;
in vec3 VertexTangent;

// Red Green Blue Depth
layout (location = 0) out vec4 OutAlbedo;
layout (location = 1) out vec4 OutNormal;
// layout (location = 2) out vec3 specularOut;
// layout (location = 3) out vec3 glossyOut;

vec4 GetAlbedoColor()
{
    return AlbedoColor * texture( AlbedoTexture, VertexUV );
}


vec3 DecodeNormal( vec3 normalC )
{
    return 2.0 * normalC - 1.0;
}


vec3 GetBumpedNormal()
{
    vec2 NormalXY = DecodeNormal( NormalColor.rgb * texture( NormalTexture, VertexUV ).rgb ).rg;
    float NormalZ = sqrt(1.0 - dot( NormalXY, NormalXY ) );
    return vec3( NormalXY, NormalZ );
}


vec3 EncodeNormal( vec3 Normal3d )
{
    return Normal3d.xyz * 0.5 + 0.5;
}


void main()
{
    OutAlbedo.rgb   = GetAlbedoColor().rgb;
    // OutAlbedo.rgb   = AlbedoColor.rgb;

    OutAlbedo.a     = VertexPos.z / ZFar;

    vec3 NormalZ    = normalize( VertexNormal );
    vec3 NormalX    = normalize( VertexTangent );
    // normally cross( NormalX, NormalZ ); but we flipped our normal for textures
    vec3 NormalY    = cross( NormalX, NormalZ ); 

    // needed to move the tangents from tangent space to object space
    mat3 TBN       = mat3( NormalX, NormalY, NormalZ );

    OutNormal.rgb   = EncodeNormal( TBN * GetBumpedNormal() );
    OutNormal.a     = 1;
}
