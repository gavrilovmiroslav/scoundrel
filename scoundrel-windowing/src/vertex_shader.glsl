
#version 440
layout (location = 0) in vec2 a_VertexPosition;
layout (location = 1) in vec2 a_TextureCoord;

uniform mat4 u_Viewport;
uniform mat4 u_Projection;
uniform mat4 u_Camera;
uniform vec2 u_GlyphSize;
uniform float u_GlyphScale;
uniform vec2 u_WindowSize;

out highp vec2 v_TextureCoord;
out mediump vec2 v_GlyphSize;
out mediump vec2 v_WindowSize;
out mediump float v_InstanceId;

void main() {
    v_WindowSize = u_WindowSize;
    v_GlyphSize = u_GlyphSize;

    v_InstanceId = gl_InstanceID;
    v_TextureCoord = a_TextureCoord;

    vec2 countPerLine = u_WindowSize / (u_GlyphSize * u_GlyphScale * 2);
    vec2 offset = vec2(mod(v_InstanceId, countPerLine.x), floor(v_InstanceId / countPerLine.x));
    gl_Position = u_Projection * u_Viewport * u_Camera * vec4((a_VertexPosition + offset + vec2(0.5, 0.5)) * u_GlyphSize * u_GlyphScale, 0.0, 1.0);
}
