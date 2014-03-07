#version 410 core

uniform mat4 mvp_matrix         = mat4(0.0);
uniform mat4 model_matrix       = mat4(0.0);
uniform mat4 view_matrix        = mat4(0.0);

out mat4 ViewMatrix;
out vec3 VertexPosition;
in vec3 vposition;

void main()
{
    VertexPosition = vec3(view_matrix * model_matrix * vec4(vposition, 1.0));
    ViewMatrix     = view_matrix;
    gl_Position    = mvp_matrix * vec4( vposition, 1.0 );
}
