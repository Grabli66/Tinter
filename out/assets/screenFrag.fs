#version 330

in vec2 fragTexCoord;
in vec4 fragColor;

out vec4 finalColor;

uniform float screenWidth;
uniform float screenHeight;
uniform float seconds;

void main()
{
    vec2 u_resolution = vec2(screenWidth,screenHeight);
    vec2 uv = gl_FragCoord.xy/u_resolution;
    vec2 pos = 0.5 - uv;
    float s1 = pos.x*pos.x*sin(seconds*3.0);
    float s2 = -pos.y*pos.y*sin(-seconds*2.0);
    float s = step(0.01,s1+s2);
    finalColor = vec4(s/0.5, 0.05, 0.02, 1.0);
}
