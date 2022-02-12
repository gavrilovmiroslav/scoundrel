#version 440
layout (location = 0) in vec2 a_VertexPosition;
layout (location = 1) in vec2 a_TextureCoord;

uniform mat4 u_Projection;
uniform mat4 u_Viewport;
uniform mat4 u_Camera;
uniform vec2 u_WindowSize;
uniform vec2 u_GlyphSize;

out highp vec2 v_TextureCoord;
out vec2 v_GlyphSize;
out float v_InstanceId;

void main() {
    v_GlyphSize = u_GlyphSize;
    v_InstanceId = gl_InstanceID;
    v_TextureCoord = u_GlyphSize + u_WindowSize + a_TextureCoord;
    gl_Position = u_Projection * u_Viewport * u_Camera * vec4(a_VertexPosition +
        vec2(mod(v_InstanceId, u_GlyphSize.x), floor(v_InstanceId / u_GlyphSize.x)), 0.0, 1.0);
}