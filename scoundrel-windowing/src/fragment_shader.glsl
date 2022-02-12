#version 440

in highp vec2 v_TextureCoord;
in vec2 v_GlyphSize;
in float v_InstanceId;

out vec4 color;

void main() {
    color = vec4(v_InstanceId / (v_GlyphSize.x * v_GlyphSize.y), v_TextureCoord.x, v_TextureCoord.y, 0.0);
}