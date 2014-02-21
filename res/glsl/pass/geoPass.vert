#version 330 core

uniform mat4 view_matrix        = mat4(0.0);
uniform mat4 projection_matrix  = mat4(0.0);
uniform mat4 vp_matrix          = mat4(0.0);
uniform mat4 model_matrix       = mat4(0.0);
uniform mat3 normal_matrix      = mat3(0.0);


in vec3 vposition;
in vec3 vnormal;
// in vec2 vtexture;
// in vec4 in_vert_color;

out vec3 VertexNormal;
out vec2 VertexTex;
out vec3 VertexPosition;

void main()
{
    mat4 modelview_matrix   = view_matrix * model_matrix;
    mat4 mvp_matrix         = vp_matrix * model_matrix;
    
    VertexPosition = vec3( modelview_matrix * vec4(vposition, 1.0) );
    VertexNormal   = normalize( normal_matrix * vnormal );
    
    // interpolated_color = in_vert_color;
    // VertexTex          = vtexture;

    gl_Position = mvp_matrix * vec4(vposition, 1.0);
}
