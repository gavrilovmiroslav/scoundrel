#version 440

flat struct Glyph {
    uint symbol;
    uint foreground;
    uint background;
};

in highp vec2 v_TextureCoord;
in mediump vec2 v_InputFontGlyphSize;
in mediump vec2 v_InputFontBitmapSize;
in mediump float v_InstanceId;
flat in Glyph v_Glyph;

uniform sampler2D s_sourceTexture;

out vec4 color;

void main() {
    vec2 glyphsInBitmapRatio = v_InputFontBitmapSize / v_InputFontGlyphSize;
    vec2 glyphScalingFactor = v_InputFontGlyphSize / v_InputFontBitmapSize;
    vec2 glyphPosition = vec2(mod(v_Glyph.symbol, glyphsInBitmapRatio.x), floor(v_Glyph.symbol / glyphsInBitmapRatio.x));
    vec4 texel = texture(s_sourceTexture, v_TextureCoord + glyphPosition * glyphScalingFactor);

    vec4 fore = unpackUnorm4x8(v_Glyph.foreground);
    vec4 back = unpackUnorm4x8(v_Glyph.background);

    if (texel.a > 1.0) discard;

//    color = vec4(v_TextureCoord.xy, 1.0, 1.0);
//    color = vec4(glyphPosition / 2000.0, 1.0, 1.0);
//    color = vec4(1.0, 1.0, 1.0, 1.0);
//    color = vec4(glyphsInBitmapRatio / 255.0, 1.0, 1.0);
//    color = texel;
    color = vec4((v_TextureCoord + glyphPosition) * glyphScalingFactor, 1.0, 1.0);
}