#version 410 core


uniform samplerCube SkyTexture;
uniform float intensity;
in vec3 VertexTex;

// Red Green Blue Depth
layout (location = 0) out vec4 pixelColor;

void main()
{
    SkyTexture;
    pixelColor.rgb   = intensity * texture(SkyTexture, VertexTex).rgb;
    pixelColor.a     = 1.0;
}

