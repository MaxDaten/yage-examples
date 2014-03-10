#version 410 core

// in vec4 interpolated_color;
#define ZBUFFER_DEPTH 1
uniform vec2 ZNearFar;
uniform vec2 ZProjRatio;

uniform sampler2D AlbedoTexture;
uniform sampler2D NormalTexture;
uniform sampler2D DepthTexture;

uniform vec3 lightPosition;    
uniform vec3 lightRadius;    
uniform vec4 lightSpecularColor;
uniform vec4 lightDiffuseColor;
uniform vec4 lightAmbientColor;

uniform ivec2 ViewportDim;

in mat4 ViewSpace;
in vec3 VertexPosVS;

const float specular_exponent = 15.0;

float zNear = ZNearFar.x;
float zFar = ZNearFar.y;


layout (location = 0) out vec4 pixelColor;

float LinearDepth (float z)
{
    return (ZProjRatio.y / (z - ZProjRatio.x));
}

void main()
{
    vec2 st;
    st = gl_FragCoord.xy / ViewportDim.xy;
    
    // the channel for albedo rgb + distance from View
    vec4 albedoCh           = texture( AlbedoTexture, st ).rgba;
    
    // the lit pixel albedo color 
    vec3 pixel_albedo       = albedoCh.rgb;

    // distance from view position (View)
    // float zzz = ;

    #ifdef ZBUFFER_DEPTH
    float depth             = texture(DepthTexture, st).x;
    float linearDepth       = LinearDepth(depth);
    vec3 viewRay            = vec3(VertexPosVS.xy / VertexPosVS.z, 1.0);
    #else
    DepthTexture;
    float linearDepth       = albedoCh.a;
    vec3 viewRay            = vec3(VertexPosVS.xy * (zFar / VertexPosVS.z), zFar);
    #endif

    // retrieve the normal of the lit pixel
    vec3 normalVS  = texture( NormalTexture, st ).rgb * 2.0 - 1.0;

    // world light position to View space
    vec3 lightPosVS = vec3(ViewSpace * vec4(lightPosition, 1.0));
    // vec3 light_pos_View = lightPosition;

    // extrapolate the View space position of the pixel to the zFar plane
    vec3 pixelPosVS     = viewRay * linearDepth;
    
    // direction from the lit pixel to the light source
    vec3 toLightDir = normalize(lightPosVS - pixelPosVS);

    float lambertian = clamp(dot(normalVS, toLightDir), 0.0, 1.0);
    float specular = 0.0;
    if (lambertian > 0.0) {
        // direction from pixel to View
        vec3 viewDir    = normalize(-pixelPosVS);
        vec3 halfDir    = normalize(toLightDir + viewDir);
        float specAngle = clamp(dot(halfDir, normalVS), 0.0, 1.0);
        specular = pow(specAngle, specular_exponent);
    }

    float dist_2d = distance (lightPosVS, pixelPosVS);
    float atten_factor = 1.0/(1.0 + dist_2d + 1 * dist_2d * dist_2d);
    pixelColor =  vec4( atten_factor * pixel_albedo * (
                 lightAmbientColor.rgb 
               + lambertian * lightDiffuseColor.rgb 
               + specular * lightSpecularColor.rgb
               ), 1.0);

    lightPosition;    
    lightRadius;    
    lightSpecularColor;    
}

