#version 410 core

uniform mat4 view_matrix        = mat4(0.0);
uniform mat4 projection_matrix  = mat4(0.0);
uniform mat4 vp_matrix          = mat4(0.0);
uniform mat4 model_matrix       = mat4(0.0);
uniform mat3 normal_matrix      = mat3(0.0);


in vec3 vposition;
in vec2 vtexture;
in vec3 vnormal;
in vec3 vtangent;
// in vec4 in_vert_color;

out vec2 VertexTex;
out vec3 VertexNormal;
out vec3 VertexTangent;

void main()
{
    mat4 modelview_matrix   = view_matrix * model_matrix;
    mat4 mvp_matrix         = vp_matrix * model_matrix;
    
    VertexTex               = vec2( vtexture.s, abs(vtexture.t - 1.0) );
    VertexNormal            = normal_matrix * vnormal;
    VertexTangent           = normal_matrix * vtangent;

    gl_Position = mvp_matrix * vec4(vposition, 1.0);
    projection_matrix;
    normal_matrix;
}
