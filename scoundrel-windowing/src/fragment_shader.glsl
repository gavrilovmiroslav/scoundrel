#version 440

flat struct Glyph {
    int symbol;
    ivec3 foreground;
    ivec3 background;
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
    vec2 glyphPosition = vec2(mod(float(v_Glyph.symbol), glyphsInBitmapRatio.x), floor(float(v_Glyph.symbol) / glyphsInBitmapRatio.x));
    vec4 texel = texture(s_sourceTexture, (v_TextureCoord + glyphPosition)  * glyphScalingFactor);

    float isAlphaDiscarded = float(texel.a < 1.0);
    if (texel.a < 1.0) discard;
    color = vec4(1.0, 1.0, 1.0, 1.0) + vec4(v_Glyph.foreground, 1.0);
//    color = vec4(vec3(float(v_Glyph.background.x), float(v_Glyph.background.y), float(v_Glyph.background.z))  * isAlphaDiscarded
  //          + vec3(float(v_Glyph.foreground.x), float(v_Glyph.foreground.y), float(v_Glyph.foreground.z)) * (1.0 - isAlphaDiscarded), 1.0);
}