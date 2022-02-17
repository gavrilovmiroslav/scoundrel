#version 440

flat struct Glyph {
    int symbol;
    ivec3 foreground;
    ivec3 background;
};

layout (location = 0) in vec2 a_VertexPosition;
layout (location = 1) in vec2 a_TextureCoord;
layout (location = 2) in int a_GlyphSymbol;
layout (location = 3) in ivec3 a_GlyphForeground;
layout (location = 4) in ivec3 a_GlyphBackground;

uniform mat4 u_Projection;
uniform mat4 u_Viewport;
uniform mat4 u_Camera;
uniform vec2 u_InputFontBitmapSize;
uniform vec2 u_InputFontGlyphSize;
uniform vec2 u_OutputGlyphScale;
uniform vec2 u_WindowSize;

out highp vec2 v_TextureCoord;
out mediump vec2 v_InputFontGlyphSize;
out mediump vec2 v_InputFontBitmapSize;
out mediump float v_InstanceId;
flat out Glyph v_Glyph;

void main() {
    v_InputFontGlyphSize = u_InputFontGlyphSize;
    v_InputFontBitmapSize = u_InputFontBitmapSize;
    v_TextureCoord = a_TextureCoord;
    v_InstanceId = gl_InstanceID;
    v_Glyph.symbol = a_GlyphSymbol;
    v_Glyph.foreground = a_GlyphForeground;
    v_Glyph.background = a_GlyphBackground;

    vec2 glyphSize = v_InputFontGlyphSize * u_OutputGlyphScale;
    vec2 countPerLine = vec2(u_WindowSize.x / glyphSize.x, u_WindowSize.y / glyphSize.y);
    vec2 quadPosition = vec2(mod(v_InstanceId, countPerLine.x), floor(v_InstanceId / countPerLine.x));
    gl_Position = u_Projection * u_Viewport * u_Camera * vec4((a_VertexPosition + quadPosition + vec2(0.5, 0.5)) * glyphSize, 0.0, 1.0);
}
