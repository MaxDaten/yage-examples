#version 330 core

// in vec4 interpolated_color;

uniform sampler2D textures;
uniform vec3 global_light_direction = normalize(vec3(0.0, 1.0, 1.0)); // should be the other direction
uniform vec3 global_light_intensity = vec3(1.0, 1.0, 1.0);

in vec2 vertex_uv;
in vec3 vertex_normal;

layout (location = 0) out vec3 albedoColor;
layout (location = 1) out vec3 normal;
layout (location = 2) out vec3 specularColor;
layout (location = 3) out vec3 glossyColor;

void main()
{

    // float cosLight    = clamp( dot( vertex_normal, global_light_direction ), 0, 1 );
    // vec3 lightedColor = global_light_intensity * cosLight;
    vec4 color        = texture( textures, vertex_uv );// * vec4(lightedColor, 1.0);
    
    albedoColor         = color.rgb;
    normal              = vertex_normal.xyz;
    specularColor       = color.rgb;
    glossyColor         = color.rgb;
}

