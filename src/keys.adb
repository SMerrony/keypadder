--  SPDX-License-Identifier: GPL-3.0-or-later
--  SPDX-FileCopyrightText:  Copyright 2023 Stephen Merrony

with Ada.Strings.Fixed;
with Ada.Text_IO;

package body Keys is

   procedure Setup_Key_Map is
   begin
      --  Injectable key codes based on include/linux/input-event-codes.h
      Keys_M.Include ("ESC", 1);
      Keys_M.Include ("1", 2);
      Keys_M.Include ("2", 3);
      Keys_M.Include ("3", 4);
      Keys_M.Include ("4", 5);
      Keys_M.Include ("5", 6);
      Keys_M.Include ("6", 7);
      Keys_M.Include ("7", 8);
      Keys_M.Include ("8", 9);
      Keys_M.Include ("9", 10);
      Keys_M.Include ("0", 11);
      Keys_M.Include ("MINUS", 12);
      Keys_M.Include ("EQUAL", 13);
      Keys_M.Include ("BACKSPACE", 14);
      Keys_M.Include ("TAB", 15);
      Keys_M.Include ("Q", 16);
      Keys_M.Include ("W", 17);
      Keys_M.Include ("E", 18);
      Keys_M.Include ("R", 19);
      Keys_M.Include ("T", 20);
      Keys_M.Include ("Y", 21);
      Keys_M.Include ("U", 22);
      Keys_M.Include ("I", 23);
      Keys_M.Include ("O", 24);
      Keys_M.Include ("P", 25);
      Keys_M.Include ("LEFTBRACE", 26);
      Keys_M.Include ("RIGHTBRACE", 27);
      Keys_M.Include ("ENTER", 28);
      Keys_M.Include ("LEFTCTRL", 29);
      Keys_M.Include ("A", 30);
      Keys_M.Include ("S", 31);
      Keys_M.Include ("D", 32);
      Keys_M.Include ("F", 33);
      Keys_M.Include ("G", 34);
      Keys_M.Include ("H", 35);
      Keys_M.Include ("J", 36);
      Keys_M.Include ("K", 37);
      Keys_M.Include ("L", 38);
      Keys_M.Include ("SEMICOLON", 39);
      Keys_M.Include ("APOSTROPHE", 40);
      Keys_M.Include ("GRAVE", 41);
      Keys_M.Include ("LEFTSHIFT", 42);
      Keys_M.Include ("BACKSLASH", 43);
      Keys_M.Include ("Z", 44);
      Keys_M.Include ("X", 45);
      Keys_M.Include ("C", 46);
      Keys_M.Include ("V", 47);
      Keys_M.Include ("B", 48);
      Keys_M.Include ("N", 49);
      Keys_M.Include ("M", 50);
      Keys_M.Include ("COMMA", 51);
      Keys_M.Include ("DOT", 52);
      Keys_M.Include ("SLASH", 53);
      Keys_M.Include ("RIGHTSHIFT", 54);
      Keys_M.Include ("KPASTERISK", 55);
      Keys_M.Include ("LEFTALT", 56);
      Keys_M.Include ("SPACE", 57);
      Keys_M.Include ("CAPSLOCK", 58);
      Keys_M.Include ("F1", 59);
      Keys_M.Include ("F2", 60);
      Keys_M.Include ("F3", 61);
      Keys_M.Include ("F4", 62);
      Keys_M.Include ("F5", 63);
      Keys_M.Include ("F6", 64);
      Keys_M.Include ("F7", 65);
      Keys_M.Include ("F8", 66);
      Keys_M.Include ("F9", 67);
      Keys_M.Include ("F10", 68);
      Keys_M.Include ("NUMLOCK", 69);
      Keys_M.Include ("SCROLLLOCK", 70);
      Keys_M.Include ("KP7", 71);
      Keys_M.Include ("KP8", 72);
      Keys_M.Include ("KP9", 73);
      Keys_M.Include ("KPMINUS", 74);
      Keys_M.Include ("KP4", 75);
      Keys_M.Include ("KP5", 76);
      Keys_M.Include ("KP6", 77);
      Keys_M.Include ("KPPLUS", 78);
      Keys_M.Include ("KP1", 79);
      Keys_M.Include ("KP2", 80);
      Keys_M.Include ("KP3", 81);
      Keys_M.Include ("KP0", 82);
      Keys_M.Include ("KPDOT", 83);
      Keys_M.Include ("ZENKAKUHANKAKU", 85);
      Keys_M.Include ("102ND", 86);
      Keys_M.Include ("F11", 87);
      Keys_M.Include ("F12", 88);
      Keys_M.Include ("RO", 89);
      Keys_M.Include ("KATAKANA", 90);
      Keys_M.Include ("HIRAGANA", 91);
      Keys_M.Include ("HENKAN", 92);
      Keys_M.Include ("KATAKANAHIRAGANA", 93);
      Keys_M.Include ("MUHENKAN", 94);
      Keys_M.Include ("KPJPCOMMA", 95);
      Keys_M.Include ("KPENTER", 96);
      Keys_M.Include ("RIGHTCTRL", 97);
      Keys_M.Include ("KPSLASH", 98);
      Keys_M.Include ("SYSRQ", 99);
      Keys_M.Include ("RIGHTALT", 100);
      Keys_M.Include ("LINEFEED", 101);
      Keys_M.Include ("HOME", 102);
      Keys_M.Include ("UP", 103);
      Keys_M.Include ("PAGEUP", 104);
      Keys_M.Include ("LEFT", 105);
      Keys_M.Include ("RIGHT", 106);
      Keys_M.Include ("END", 107);
      Keys_M.Include ("DOWN", 108);
      Keys_M.Include ("PAGEDOWN", 109);
      Keys_M.Include ("INSERT", 110);
      Keys_M.Include ("DELETE", 111);
      Keys_M.Include ("MACRO", 112);
      Keys_M.Include ("MUTE", 113);
      Keys_M.Include ("VOLUMEDOWN", 114);
      Keys_M.Include ("VOLUMEUP", 115);
      Keys_M.Include ("POWER", 116);
      Keys_M.Include ("KPEQUAL", 117);
      Keys_M.Include ("KPPLUSMINUS", 118);
      Keys_M.Include ("PAUSE", 119);
      Keys_M.Include ("SCALE", 120);
      Keys_M.Include ("KPCOMMA", 121);
      Keys_M.Include ("HANGEUL", 122);
      Keys_M.Include ("HANGUEL", 122);
      Keys_M.Include ("HANJA", 123);
      Keys_M.Include ("YEN", 124);
      Keys_M.Include ("LEFTMETA", 125);
      Keys_M.Include ("RIGHTMETA", 126);
      Keys_M.Include ("COMPOSE", 127);
      Keys_M.Include ("STOP", 128);
      Keys_M.Include ("AGAIN", 129);
      Keys_M.Include ("PROPS", 130);
      Keys_M.Include ("UNDO", 131);
      Keys_M.Include ("FRONT", 132);
      Keys_M.Include ("COPY", 133);
      Keys_M.Include ("OPEN", 134);
      Keys_M.Include ("PASTE", 135);
      Keys_M.Include ("FIND", 136);
      Keys_M.Include ("CUT", 137);
      Keys_M.Include ("HELP", 138);
      Keys_M.Include ("MENU", 139);
      Keys_M.Include ("CALC", 140);
      Keys_M.Include ("SETUP", 141);
      Keys_M.Include ("SLEEP", 142);
      Keys_M.Include ("WAKEUP", 143);
      Keys_M.Include ("FILE", 144);
      Keys_M.Include ("SENDFILE", 145);
      Keys_M.Include ("DELETEFILE", 146);
      Keys_M.Include ("XFER", 147);
      Keys_M.Include ("PROG1", 148);
      Keys_M.Include ("PROG2", 149);
      Keys_M.Include ("WWW", 150);
      Keys_M.Include ("MSDOS", 151);
      Keys_M.Include ("COFFEE", 152);
      Keys_M.Include ("SCREENLOCK", 152);
      Keys_M.Include ("ROTATE_DISPLAY", 153);
      Keys_M.Include ("DIRECTION", 153);
      Keys_M.Include ("CYCLEWINDOWS", 154);
      Keys_M.Include ("MAIL", 155);
      Keys_M.Include ("BOOKMARKS", 156);
      Keys_M.Include ("COMPUTER", 157);
      Keys_M.Include ("BACK", 158);
      Keys_M.Include ("FORWARD", 159);
      Keys_M.Include ("CLOSECD", 160);
      Keys_M.Include ("EJECTCD", 161);
      Keys_M.Include ("EJECTCLOSECD", 162);
      Keys_M.Include ("NEXTSONG", 163);
      Keys_M.Include ("PLAYPAUSE", 164);
      Keys_M.Include ("PREVIOUSSONG", 165);
      Keys_M.Include ("STOPCD", 166);
      Keys_M.Include ("RECORD", 167);
      Keys_M.Include ("REWIND", 168);
      Keys_M.Include ("PHONE", 169);
      Keys_M.Include ("ISO", 170);
      Keys_M.Include ("CONFIG", 171);
      Keys_M.Include ("HOMEPAGE", 172);
      Keys_M.Include ("REFRESH", 173);
      Keys_M.Include ("EXIT", 174);
      Keys_M.Include ("MOVE", 175);
      Keys_M.Include ("EDIT", 176);
      Keys_M.Include ("SCROLLUP", 177);
      Keys_M.Include ("SCROLLDOWN", 178);
      Keys_M.Include ("KPLEFTPAREN", 179);
      Keys_M.Include ("KPRIGHTPAREN", 180);
      Keys_M.Include ("NEW", 181);
      Keys_M.Include ("REDO", 182);
      Keys_M.Include ("F13", 183);
      Keys_M.Include ("F14", 184);
      Keys_M.Include ("F15", 185);
      Keys_M.Include ("F16", 186);
      Keys_M.Include ("F17", 187);
      Keys_M.Include ("F18", 188);
      Keys_M.Include ("F19", 189);
      Keys_M.Include ("F20", 190);
      Keys_M.Include ("F21", 191);
      Keys_M.Include ("F22", 192);
      Keys_M.Include ("F23", 193);
      Keys_M.Include ("F24", 194);
      Keys_M.Include ("PLAYCD", 200);
      Keys_M.Include ("PAUSECD", 201);
      Keys_M.Include ("PROG3", 202);
      Keys_M.Include ("PROG4", 203);
      Keys_M.Include ("ALL_APPLICATIONS", 204);
      Keys_M.Include ("DASHBOARD", 204);
      Keys_M.Include ("SUSPEND", 205);
      Keys_M.Include ("CLOSE", 206);
      Keys_M.Include ("PLAY", 207);
      Keys_M.Include ("FASTFORWARD", 208);
      Keys_M.Include ("BASSBOOST", 209);
      Keys_M.Include ("PRINT", 210);
      Keys_M.Include ("HP", 211);
      Keys_M.Include ("CAMERA", 212);
      Keys_M.Include ("SOUND", 213);
      Keys_M.Include ("QUESTION", 214);
      Keys_M.Include ("EMAIL", 215);
      Keys_M.Include ("CHAT", 216);
      Keys_M.Include ("SEARCH", 217);
      Keys_M.Include ("CONNECT", 218);
      Keys_M.Include ("FINANCE", 219);
      Keys_M.Include ("SPORT", 220);
      Keys_M.Include ("SHOP", 221);
      Keys_M.Include ("ALTERASE", 222);
      Keys_M.Include ("CANCEL", 223);
      Keys_M.Include ("BRIGHTNESSDOWN", 224);
      Keys_M.Include ("BRIGHTNESSUP", 225);
      Keys_M.Include ("MEDIA", 226);
      Keys_M.Include ("SWITCHVIDEOMODE", 227);
      Keys_M.Include ("KBDILLUMTOGGLE", 228);
      Keys_M.Include ("KBDILLUMDOWN", 229);
      Keys_M.Include ("KBDILLUMUP", 230);
      Keys_M.Include ("SEND", 231);
      Keys_M.Include ("REPLY", 232);
      Keys_M.Include ("FORWARDMAIL", 233);
      Keys_M.Include ("SAVE", 234);
      Keys_M.Include ("DOCUMENTS", 235);
      Keys_M.Include ("BATTERY", 236);
      Keys_M.Include ("BLUETOOTH", 237);
      Keys_M.Include ("WLAN", 238);
      Keys_M.Include ("UWB", 239);
      Keys_M.Include ("UNKNOWN", 240);
      Keys_M.Include ("VIDEO_NEXT", 241);
      Keys_M.Include ("VIDEO_PREV", 242);
      Keys_M.Include ("BRIGHTNESS_CYCLE", 243);
      Keys_M.Include ("BRIGHTNESS_AUTO", 244);
      Keys_M.Include ("BRIGHTNESS_ZERO", 244);
      Keys_M.Include ("DISPLAY_OFF", 245);
      Keys_M.Include ("WWAN", 246);
      Keys_M.Include ("WIMAX", 246);
      Keys_M.Include ("RFKILL", 247);
      Keys_M.Include ("MICMUTE", 248);
      Keys_M.Include ("OK", 16#160#);
      Keys_M.Include ("SELECT", 16#161#);
      Keys_M.Include ("GOTO", 16#162#);
      Keys_M.Include ("CLEAR", 16#163#);
      Keys_M.Include ("POWER2", 16#164#);
      Keys_M.Include ("OPTION", 16#165#);
      Keys_M.Include ("INFO", 16#166#);
      Keys_M.Include ("TIME", 16#167#);
      Keys_M.Include ("VENDOR", 16#168#);
      Keys_M.Include ("ARCHIVE", 16#169#);
      Keys_M.Include ("PROGRAM", 16#16a#);
      Keys_M.Include ("CHANNEL", 16#16b#);
      Keys_M.Include ("FAVORITES", 16#16c#);
      Keys_M.Include ("EPG", 16#16d#);
      Keys_M.Include ("PVR", 16#16e#);
      Keys_M.Include ("MHP", 16#16f#);
      Keys_M.Include ("LANGUAGE", 16#170#);
      Keys_M.Include ("TITLE", 16#171#);
      Keys_M.Include ("SUBTITLE", 16#172#);
      Keys_M.Include ("ANGLE", 16#173#);
      Keys_M.Include ("FULL_SCREEN", 16#174#);
      Keys_M.Include ("ZOOM", 16#174#);
      Keys_M.Include ("MODE", 16#175#);
      Keys_M.Include ("KEYBOARD", 16#176#);
      Keys_M.Include ("ASPECT_RATIO", 16#177#);
      Keys_M.Include ("SCREEN", 16#177#);
      Keys_M.Include ("PC", 16#178#);
      Keys_M.Include ("TV", 16#179#);
      Keys_M.Include ("TV2", 16#17a#);
      Keys_M.Include ("VCR", 16#17b#);
      Keys_M.Include ("VCR2", 16#17c#);
      Keys_M.Include ("SAT", 16#17d#);
      Keys_M.Include ("SAT2", 16#17e#);
      Keys_M.Include ("CD", 16#17f#);
      Keys_M.Include ("TAPE", 16#180#);
      Keys_M.Include ("RADIO", 16#181#);
      Keys_M.Include ("TUNER", 16#182#);
      Keys_M.Include ("PLAYER", 16#183#);
      Keys_M.Include ("TEXT", 16#184#);
      Keys_M.Include ("DVD", 16#185#);
      Keys_M.Include ("AUX", 16#186#);
      Keys_M.Include ("MP3", 16#187#);
      Keys_M.Include ("AUDIO", 16#188#);
      Keys_M.Include ("VIDEO", 16#189#);
      Keys_M.Include ("DIRECTORY", 16#18a#);
      Keys_M.Include ("LIST", 16#18b#);
      Keys_M.Include ("MEMO", 16#18c#);
      Keys_M.Include ("CALENDAR", 16#18d#);
      Keys_M.Include ("RED", 16#18e#);
      Keys_M.Include ("GREEN", 16#18f#);
      Keys_M.Include ("YELLOW", 16#190#);
      Keys_M.Include ("BLUE", 16#191#);
      Keys_M.Include ("CHANNELUP", 16#192#);
      Keys_M.Include ("CHANNELDOWN", 16#193#);
      Keys_M.Include ("FIRST", 16#194#);
      Keys_M.Include ("LAST", 16#195#);
      Keys_M.Include ("AB", 16#196#);
      Keys_M.Include ("NEXT", 16#197#);
      Keys_M.Include ("RESTART", 16#198#);
      Keys_M.Include ("SLOW", 16#199#);
      Keys_M.Include ("SHUFFLE", 16#19a#);
      Keys_M.Include ("BREAK", 16#19b#);
      Keys_M.Include ("PREVIOUS", 16#19c#);
      Keys_M.Include ("DIGITS", 16#19d#);
      Keys_M.Include ("TEEN", 16#19e#);
      Keys_M.Include ("TWEN", 16#19f#);
      Keys_M.Include ("VIDEOPHONE", 16#1a0#);
      Keys_M.Include ("GAMES", 16#1a1#);
      Keys_M.Include ("ZOOMIN", 16#1a2#);
      Keys_M.Include ("ZOOMOUT", 16#1a3#);
      Keys_M.Include ("ZOOMRESET", 16#1a4#);
      Keys_M.Include ("WORDPROCESSOR", 16#1a5#);
      Keys_M.Include ("EDITOR", 16#1a6#);
      Keys_M.Include ("SPREADSHEET", 16#1a7#);
      Keys_M.Include ("GRAPHICSEDITOR", 16#1a8#);
      Keys_M.Include ("PRESENTATION", 16#1a9#);
      Keys_M.Include ("DATABASE", 16#1aa#);
      Keys_M.Include ("NEWS", 16#1ab#);
      Keys_M.Include ("VOICEMAIL", 16#1ac#);
      Keys_M.Include ("ADDRESSBOOK", 16#1ad#);
      Keys_M.Include ("MESSENGER", 16#1ae#);
      Keys_M.Include ("DISPLAYTOGGLE", 16#1af#);
      Keys_M.Include ("BRIGHTNESS_TOGGLE", 16#1af#);
      Keys_M.Include ("SPELLCHECK", 16#1b0#);
      Keys_M.Include ("LOGOFF", 16#1b1#);
      Keys_M.Include ("DOLLAR", 16#1b2#);
      Keys_M.Include ("EURO", 16#1b3#);
      Keys_M.Include ("FRAMEBACK", 16#1b4#);
      Keys_M.Include ("FRAMEFORWARD", 16#1b5#);
      Keys_M.Include ("CONTEXT_MENU", 16#1b6#);
      Keys_M.Include ("MEDIA_REPEAT", 16#1b7#);
      Keys_M.Include ("10CHANNELSUP", 16#1b8#);
      Keys_M.Include ("10CHANNELSDOWN", 16#1b9#);
      Keys_M.Include ("IMAGES", 16#1ba#);
      Keys_M.Include ("NOTIFICATION_CENTER", 16#1bc#);
      Keys_M.Include ("PICKUP_PHONE", 16#1bd#);
      Keys_M.Include ("HANGUP_PHONE", 16#1be#);
      Keys_M.Include ("DEL_EOL", 16#1c0#);
      Keys_M.Include ("DEL_EOS", 16#1c1#);
      Keys_M.Include ("INS_LINE", 16#1c2#);
      Keys_M.Include ("DEL_LINE", 16#1c3#);
      Keys_M.Include ("FN", 16#1d0#);
      Keys_M.Include ("FN_ESC", 16#1d1#);
      Keys_M.Include ("FN_F1", 16#1d2#);
      Keys_M.Include ("FN_F2", 16#1d3#);
      Keys_M.Include ("FN_F3", 16#1d4#);
      Keys_M.Include ("FN_F4", 16#1d5#);
      Keys_M.Include ("FN_F5", 16#1d6#);
      Keys_M.Include ("FN_F6", 16#1d7#);
      Keys_M.Include ("FN_F7", 16#1d8#);
      Keys_M.Include ("FN_F8", 16#1d9#);
      Keys_M.Include ("FN_F9", 16#1da#);
      Keys_M.Include ("FN_F10", 16#1db#);
      Keys_M.Include ("FN_F11", 16#1dc#);
      Keys_M.Include ("FN_F12", 16#1dd#);
      Keys_M.Include ("FN_1", 16#1de#);
      Keys_M.Include ("FN_2", 16#1df#);
      Keys_M.Include ("FN_D", 16#1e0#);
      Keys_M.Include ("FN_E", 16#1e1#);
      Keys_M.Include ("FN_F", 16#1e2#);
      Keys_M.Include ("FN_S", 16#1e3#);
      Keys_M.Include ("FN_B", 16#1e4#);
      Keys_M.Include ("FN_RIGHT_SHIFT", 16#1e5#);
      Keys_M.Include ("BRL_DOT1", 16#1f1#);
      Keys_M.Include ("BRL_DOT2", 16#1f2#);
      Keys_M.Include ("BRL_DOT3", 16#1f3#);
      Keys_M.Include ("BRL_DOT4", 16#1f4#);
      Keys_M.Include ("BRL_DOT5", 16#1f5#);
      Keys_M.Include ("BRL_DOT6", 16#1f6#);
      Keys_M.Include ("BRL_DOT7", 16#1f7#);
      Keys_M.Include ("BRL_DOT8", 16#1f8#);
      Keys_M.Include ("BRL_DOT9", 16#1f9#);
      Keys_M.Include ("BRL_DOT10", 16#1fa#);
      Keys_M.Include ("NUMERIC_0", 16#200#);
      Keys_M.Include ("NUMERIC_1", 16#201#);
      Keys_M.Include ("NUMERIC_2", 16#202#);
      Keys_M.Include ("NUMERIC_3", 16#203#);
      Keys_M.Include ("NUMERIC_4", 16#204#);
      Keys_M.Include ("NUMERIC_5", 16#205#);
      Keys_M.Include ("NUMERIC_6", 16#206#);
      Keys_M.Include ("NUMERIC_7", 16#207#);
      Keys_M.Include ("NUMERIC_8", 16#208#);
      Keys_M.Include ("NUMERIC_9", 16#209#);
      Keys_M.Include ("NUMERIC_STAR", 16#20a#);
      Keys_M.Include ("NUMERIC_POUND", 16#20b#);
      Keys_M.Include ("NUMERIC_A", 16#20c#);
      Keys_M.Include ("NUMERIC_B", 16#20d#);
      Keys_M.Include ("NUMERIC_C", 16#20e#);
      Keys_M.Include ("NUMERIC_D", 16#20f#);
      Keys_M.Include ("CAMERA_FOCUS", 16#210#);
      Keys_M.Include ("WPS_BUTTON", 16#211#);
      Keys_M.Include ("TOUCHPAD_TOGGLE", 16#212#);
      Keys_M.Include ("TOUCHPAD_ON", 16#213#);
      Keys_M.Include ("TOUCHPAD_OFF", 16#214#);
      Keys_M.Include ("CAMERA_ZOOMIN", 16#215#);
      Keys_M.Include ("CAMERA_ZOOMOUT", 16#216#);
      Keys_M.Include ("CAMERA_UP", 16#217#);
      Keys_M.Include ("CAMERA_DOWN", 16#218#);
      Keys_M.Include ("CAMERA_LEFT", 16#219#);
      Keys_M.Include ("CAMERA_RIGHT", 16#21a#);
      Keys_M.Include ("ATTENDANT_ON", 16#21b#);
      Keys_M.Include ("ATTENDANT_OFF", 16#21c#);
      Keys_M.Include ("ATTENDANT_TOGGLE", 16#21d#);
      Keys_M.Include ("LIGHTS_TOGGLE", 16#21e#);
      Keys_M.Include ("ALS_TOGGLE", 16#230#);
      Keys_M.Include ("ROTATE_LOCK_TOGGLE", 16#231#);
      Keys_M.Include ("BUTTONCONFIG", 16#240#);
      Keys_M.Include ("TASKMANAGER", 16#241#);
      Keys_M.Include ("JOURNAL", 16#242#);
      Keys_M.Include ("CONTROLPANEL", 16#243#);
      Keys_M.Include ("APPSELECT", 16#244#);
      Keys_M.Include ("SCREENSAVER", 16#245#);
      Keys_M.Include ("VOICECOMMAND", 16#246#);
      Keys_M.Include ("ASSISTANT", 16#247#);
      Keys_M.Include ("KBD_LAYOUT_NEXT", 16#248#);
      Keys_M.Include ("EMOJI_PICKER", 16#249#);
      Keys_M.Include ("DICTATE", 16#24a#);
      Keys_M.Include ("BRIGHTNESS_MIN", 16#250#);
      Keys_M.Include ("BRIGHTNESS_MAX", 16#251#);
      Keys_M.Include ("KBDINPUTASSIST_PREV", 16#260#);
      Keys_M.Include ("KBDINPUTASSIST_NEXT", 16#261#);
      Keys_M.Include ("KBDINPUTASSIST_PREVGROUP", 16#262#);
      Keys_M.Include ("KBDINPUTASSIST_NEXTGROUP", 16#263#);
      Keys_M.Include ("KBDINPUTASSIST_ACCEPT", 16#264#);
      Keys_M.Include ("KBDINPUTASSIST_CANCEL", 16#265#);
      Keys_M.Include ("RIGHT_UP", 16#266#);
      Keys_M.Include ("RIGHT_DOWN", 16#267#);
      Keys_M.Include ("LEFT_UP", 16#268#);
      Keys_M.Include ("LEFT_DOWN", 16#269#);
      Keys_M.Include ("ROOT_MENU", 16#26a#);
      Keys_M.Include ("MEDIA_TOP_MENU", 16#26b#);
      Keys_M.Include ("NUMERIC_11", 16#26c#);
      Keys_M.Include ("NUMERIC_12", 16#26d#);
      Keys_M.Include ("AUDIO_DESC", 16#26e#);
      Keys_M.Include ("3D_MODE", 16#26f#);
      Keys_M.Include ("NEXT_FAVORITE", 16#270#);
      Keys_M.Include ("STOP_RECORD", 16#271#);
      Keys_M.Include ("PAUSE_RECORD", 16#272#);
      Keys_M.Include ("VOD", 16#273#);
      Keys_M.Include ("UNMUTE", 16#274#);
      Keys_M.Include ("FASTREVERSE", 16#275#);
      Keys_M.Include ("SLOWREVERSE", 16#276#);
      Keys_M.Include ("DATA", 16#277#);
      Keys_M.Include ("ONSCREEN_KEYBOARD", 16#278#);
      Keys_M.Include ("PRIVACY_SCREEN_TOGGLE", 16#279#);
      Keys_M.Include ("SELECTIVE_SCREENSHOT", 16#27a#);
      Keys_M.Include ("MACRO1", 16#290#);
      Keys_M.Include ("MACRO2", 16#291#);
      Keys_M.Include ("MACRO3", 16#292#);
      Keys_M.Include ("MACRO4", 16#293#);
      Keys_M.Include ("MACRO5", 16#294#);
      Keys_M.Include ("MACRO6", 16#295#);
      Keys_M.Include ("MACRO7", 16#296#);
      Keys_M.Include ("MACRO8", 16#297#);
      Keys_M.Include ("MACRO9", 16#298#);
      Keys_M.Include ("MACRO10", 16#299#);
      Keys_M.Include ("MACRO11", 16#29a#);
      Keys_M.Include ("MACRO12", 16#29b#);
      Keys_M.Include ("MACRO13", 16#29c#);
      Keys_M.Include ("MACRO14", 16#29d#);
      Keys_M.Include ("MACRO15", 16#29e#);
      Keys_M.Include ("MACRO16", 16#29f#);
      Keys_M.Include ("MACRO17", 16#2a0#);
      Keys_M.Include ("MACRO18", 16#2a1#);
      Keys_M.Include ("MACRO19", 16#2a2#);
      Keys_M.Include ("MACRO20", 16#2a3#);
      Keys_M.Include ("MACRO21", 16#2a4#);
      Keys_M.Include ("MACRO22", 16#2a5#);
      Keys_M.Include ("MACRO23", 16#2a6#);
      Keys_M.Include ("MACRO24", 16#2a7#);
      Keys_M.Include ("MACRO25", 16#2a8#);
      Keys_M.Include ("MACRO26", 16#2a9#);
      Keys_M.Include ("MACRO27", 16#2aa#);
      Keys_M.Include ("MACRO28", 16#2ab#);
      Keys_M.Include ("MACRO29", 16#2ac#);
      Keys_M.Include ("MACRO30", 16#2ad#);
      Keys_M.Include ("MACRO_RECORD_START", 16#2b0#);
      Keys_M.Include ("MACRO_RECORD_STOP", 16#2b1#);
      Keys_M.Include ("MACRO_PRESET_CYCLE", 16#2b2#);
      Keys_M.Include ("MACRO_PRESET1", 16#2b3#);
      Keys_M.Include ("MACRO_PRESET2", 16#2b4#);
      Keys_M.Include ("MACRO_PRESET3", 16#2b5#);
      Keys_M.Include ("KBD_LCD_MENU1", 16#2b8#);
      Keys_M.Include ("KBD_LCD_MENU2", 16#2b9#);
      Keys_M.Include ("KBD_LCD_MENU3", 16#2ba#);
      Keys_M.Include ("KBD_LCD_MENU4", 16#2bb#);
      Keys_M.Include ("KBD_LCD_MENU5", 16#2bc#);

   end Setup_Key_Map;

   procedure List_All_Keys is
      use Ada.Strings.Fixed;
      use Ada.Text_IO;
   begin
      Put_Line ("Mnemonic               Value");
      Put_Line ("----------------------------");
      for K in Keys_M.Iterate loop
         Put_Line (Head (Key (K), 24, ' ') & Tail (Element (K)'Image, 4, ' '));
      end loop;
      Put_Line ("----------------------------");
   end List_All_Keys;

end Keys;
