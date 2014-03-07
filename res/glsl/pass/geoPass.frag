#version 410 core

// in vec4 interpolated_color;
uniform mat3 normal_matrix      = mat3(0.0);
uniform float far;

uniform sampler2D tex_albedo;
uniform sampler2D tex_normal;
// uniform sampler2D tex_tangent;
// uniform vec3 global_light_direction = normalize(vec3(0.0, 1.0, 1.0)); // should be the other direction
// uniform vec3 global_light_intensity = vec3(1.0, 1.0, 1.0);


in vec2 VertexTex;
in vec3 VertexNormal;
in vec3 VertexTangent;

// Red Green Blue Depth
layout (location = 0) out vec4 albedoOut;
layout (location = 1) out vec4 normalOut;
// layout (location = 2) out vec3 specularOut;
// layout (location = 3) out vec3 glossyOut;

void main()
{

    // float cosLight    = clamp( dot( vertex_normal, global_light_direction ), 0, 1 );
    // vec3 lightedColor = global_light_intensity * cosLight;
    // vec4 color          = vec4(0, 1, 0, 0); // texture( textures, vertex_uv );// * vec4(lightedColor, 1.0);
    
    albedoOut.rgb   = texture(tex_albedo, VertexTex).xyz;
    albedoOut.a     = 1.0;

    vec3 normal    = normalize(VertexNormal);
    vec3 tangent   = normalize(VertexTangent);
    vec3 bitangent = cross(normal, tangent);
    mat3 tbn       = transpose(mat3(tangent, bitangent, normal));

    normalOut.rgb   = bitangent;
    normalOut.a     = 1;
    far;
    tex_albedo;
    tex_normal;
    //  specularOut     = color.rgb;
    // glossyOut       = color.rgb;
}

