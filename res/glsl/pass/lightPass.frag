#version 410 core

// in vec4 interpolated_color;
uniform vec3 eye_position;

uniform float near;
uniform float far;

uniform sampler2D tex_albedo;
uniform sampler2D tex_normal;
uniform sampler2D tex_depth;

uniform vec3 lightPosition;    
uniform vec3 lightRadius;    
uniform vec4 lightSpecularColor;
uniform vec4 lightDiffuseColor;
uniform vec4 lightAmbientColor;

uniform ivec2 viewportDim;

in mat4 ViewMatrix;
in vec3 VertexPosition;

const float specular_exponent = 15.0;

vec2 projRatio = vec2(far / (far - near), near/(near-far));
layout (location = 0) out vec4 pixelColor;

vec3 uv_to_eye(vec2 uv, float eye_z);
vec3 fetch_eye_pos(vec2 uv);
float getLinearDepth (vec2);


void main()
{
    vec2 st;
    st = gl_FragCoord.xy / viewportDim.xy;
    
    // the channel for albedo rgb + distance from eye
    vec4 albedoCh           = texture( tex_albedo, st );
    
    // the lit pixel albedo color 
    vec3 pixel_albedo       = albedoCh.rgb;

    // distance from view position (eye)
    // float zzz = ; 
    float linearDepth  = getLinearDepth(st);

    // retrieve the normal of the lit pixel
    vec3 pixel_normal  = texture( tex_normal, st ).rgb;

    // world light position to eye space
    vec3 light_pos_eye = vec3(ViewMatrix * vec4(lightPosition, 1.0));

    // extrapolate the eye space position of the pixel to the far plane
    vec3 viewRay   = vec3(VertexPosition.xy * (far / VertexPosition.z), far);
    vec3 eye_pos   = viewRay * linearDepth;
    
    // direction from the lit pixel to the light source
    vec3 light_dir = normalize(light_pos_eye - eye_pos);

    float lambertian = clamp(dot(pixel_normal, light_dir), 0.0, 1.0);
    float specular = 0.0;
    if (lambertian > 0.0) {
        // direction from pixel to eye
        vec3 eye_dir    = normalize(-eye_pos);
        vec3 halfDir    = normalize(light_dir + eye_dir);
        float specAngle = clamp(dot(halfDir, pixel_normal), 0.0, 1.0);
        specular = pow(specAngle, specular_exponent);
    }

    float dist_2d = distance (light_pos_eye, eye_pos);
    float atten_factor = 1; // / (dist_2d + 2 * dist_2d * dist_2d);
    pixelColor =  vec4( atten_factor * pixel_albedo * (
                 lightAmbientColor.rgb 
               + lambertian * lightDiffuseColor.rgb 
               + specular * lightSpecularColor.rgb ), 1.0);


    // pixelColor = vec4(pixel_normal, 1.0);
    // float z = linearizeDepth(norm_depth);
    // pixelColor = vec4(depth*2, 0, 0, 1.0);
    // pixelColor = vec4(norm_depth,0,0,1);
    // if (norm_depth < 2) { pixelColor = vec4(1, 0, 0, 1); }
    // pixelColor = vec4(normalize(-eye_pos), 1.0);
    // pixelColor = vec4(vec3(norm_depth, norm_depth, norm_depth), 1.0);
    eye_position;
    lightPosition;    
    lightRadius;    
    tex_normal;
    near;
    far;
}

float getLinearDepth (vec2 st)
{
    return (projRatio.y / texture(tex_depth, st).x - projRatio.x);
}

// float invFocalLenX       = tan(fovy * 0.5) * viewportDim.x / viewportDim.y;
// float invFocalLenY       = tan(fovy * 0.5);

// vec3 uv_to_eye(vec2 uv, float eye_z)
// {
//    uv = (uv * vec2(2.0, -2.0) - vec2(1.0, -1.0));
//    return vec3(uv * vec2(invFocalLenX, invFocalLenY) * eye_z, eye_z);
// }

// vec3 fetch_eye_pos(vec2 uv)
// {
//    float z = texture(tex_albedo, uv).a; // Depth/Normal buffer
//    return uv_to_eye(uv, z);
// }
