#version 440

in highp vec2 v_TextureCoord;
in mediump vec2 v_GlyphSize;
in mediump vec2 v_WindowSize;
in mediump float v_InstanceId;

uniform sampler2D s_sourceTexture;

out vec4 color;

void main() {
//    float id = v_InstanceId / (v_GlyphSize.x * v_GlyphSize.y) + (v_WindowSize.x - v_WindowSize.y;
    color = texture(s_sourceTexture, v_TextureCoord);
}