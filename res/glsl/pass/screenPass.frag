#version 410 core

// in vec4 interpolated_color;

uniform sampler2D ScreenTexture;
in vec2 VertexUV;

layout (location = 0) out vec4 pixelColor;

vec3 LinearToneMapping(vec3 color)
{
    color *= 16;  // Hardcoded Exposure Adjustment
    return vec3(pow(color.r, 1/2.2),pow(color.g, 1/2.2),pow(color.b, 1/2.2));
}


void main()
{
    pixelColor = texture( ScreenTexture, VertexUV );
}

