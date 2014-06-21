#version 410 core
// http://frictionalgames.blogspot.de/2012/09/tech-feature-hdr-lightning.html
// http://http.download.nvidia.com/developer/presentations/2004/6800_Leagues/6800_Leagues_HDR.pdf
// http://filmicgames.com/archives/75


uniform sampler2D ScreenTexture;
uniform sampler2D AddTexture;
uniform float InverseGamma  = 1.0 / 2.2;
uniform float Exposure      = 1.0;
uniform float ExposureBias  = 1.0;

float A = 0.15;
float B = 0.50;
float C = 0.10;
float D = 0.20;
float E = 0.02;
float F = 0.30;
float W = 11.2; // white point

in vec2 VertexUV;
layout (location = 0) out vec3 pixelColor;

//------------------------------------

vec3 pow3(vec3 x, float y)
{
    return vec3(pow(x.r, y), pow(x.g, y), pow(x.b, y));
}

vec3 inverseGamma(vec3 x)
{
    return pow3(x, InverseGamma);
}

vec3 LinearToneMapping(vec3 color)
{
    return color;
}

vec3 ReinhardToneMapping(vec3 color)
{
    color = color / (1+color);
    return color;
}

vec3 Uncharted2ToneMapping(vec3 color)
{
    return ((color*(A*color+C*B)+D*E)/(color*(A*color+B)+D*F))-E/F;
}

vec3 ToneMapping(vec3 color)
{
    // return LinearToneMapping(color);
    return ReinhardToneMapping(color);
    // return Uncharted2ToneMapping(color);
}

void main()
{
    vec3 texColor = texture( ScreenTexture, VertexUV ).rgb;

    // currently adding bloom
    texColor     += 3.0 * texture( AddTexture, VertexUV ).rgb;
    texColor     *= Exposure;

    vec3 color = ToneMapping( ExposureBias * texColor );
    vec3 whiteScale = 1.0f / ToneMapping(vec3(W));

    color *= whiteScale;

    color = inverseGamma( color );
    pixelColor = clamp(color, 0, 1);
}

