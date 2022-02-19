#version 440

vec4 hsv2rgb(vec4 c)
{
    vec4 K = vec4(1.0, 2.0 / 3.0, 1.0 / 3.0, 3.0);
    vec3 p = abs(fract(c.xxx + K.xyz) * 6.0 - K.www);
    return vec4(c.z * mix(K.xxx, clamp(p - K.xxx, 0.0, 1.0), c.y), 1.0);
}

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
    vec4 texel = texture(s_sourceTexture, (v_TextureCoord + glyphPosition) * glyphScalingFactor);

    vec4 fore = hsv2rgb(unpackUnorm4x8(v_Glyph.foreground));
    vec4 back = hsv2rgb(unpackUnorm4x8(v_Glyph.background));

    float shouldBeBackground = float(texel.a < 1.0);
    color = back * shouldBeBackground + fore * (1.0 - shouldBeBackground);
}