#version 410 core

// in vec4 interpolated_color;

uniform sampler2D ScreenTexture;
in vec2 VertexTex;

layout (location = 0) out vec4 pixelColor;

void main()
{
    pixelColor = texture( ScreenTexture, VertexTex );
}

