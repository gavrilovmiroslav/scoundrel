#version 440

in highp vec2 v_TextureCoord;
in float id;
out vec4 color;

void main() {
    color = vec4(id / (64.0 * 48.0), v_TextureCoord.x, v_TextureCoord.y, 0.0);
}