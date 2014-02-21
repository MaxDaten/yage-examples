#version 330 core

// in vec4 interpolated_color;

uniform sampler2D screen_texture;
const float blurSizeH = 1.0 / 300.0;
const float blurSizeV = 1.0 / 200.0;
const ivec2 size = ivec2(2);
const float blurArea = 25;
in vec2 VertexTex;

layout (location = 0) out vec4 pixelColor;

void main()
{
    pixelColor = texture( screen_texture, VertexTex );
    // vec4 sum = vec4(0.0);

    // for (int x = -(size.x); x <= (size.x); x++)
    //     for (int y = -(size.y); y <= (size.y); y++)
    //         sum += texture(
    //             texture,
    //             vec2(vertex_uv.x + x * blurSizeH, vertex_uv.y + y * blurSizeV)
    //         ) / blurArea;
    // pixelColor = sum;
}

