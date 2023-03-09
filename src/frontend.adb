--  SPDX-License-Identifier: GPL-3.0-or-later
--  SPDX-FileCopyrightText:  Copyright 2023 Stephen Merrony

with Ada.Exceptions;        use Ada.Exceptions;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO;           use Ada.Text_IO;

with AWS.Parameters;

with Config;   use Config;
with Injector;

package body Frontend is

   function Request_CB (Request : AWS.Status.Data) return AWS.Response.Data is
      URI : constant String := AWS.Status.URI (Request);
      Parms : AWS.Parameters.List;
      Current_Tab_Ix : Positive;
   begin
      if URI = "/" then
         return AWS.Response.Build ("text/html", Build_Main_Page (1));
      elsif URI = "/buttonpress" then
         Parms := AWS.Status.Parameters (Request);
         Decode_And_Send_Key (To_String (AWS.Parameters.Get (Parms, 1).Value),
                              To_String (AWS.Parameters.Get (Parms, 2).Value), Current_Tab_Ix);
         return AWS.Response.Build ("text/html", "OK");
      elsif URI = "/shutdown" then
         Shutting_Down := True;
         return AWS.Response.Build ("text/html", "<p>Shutting down...");
      else
         return AWS.Response.Build ("text/html", "<p>Unknown request");
      end if;
   end Request_CB;

   function Build_Main_Page (Active_Tab : Positive) return String is
      use Ada.Strings.Fixed;
      Header_HTML : constant String :=
         "<!DOCTYPE html><html><head><style>" &
         "body {min-height:100%; background-color:darkgray; color:white; overflow:scroll;}" &
         ".kp-bar {margin:0; padding:0mm} .kp-bar-item {font-size:10mm} " &
         ".kp-selector {position:absolute; height:12mm; top:1px; right:2px; font-size:10mm;} " &
         ".kp-pad {align-content:stretch;} " &
         ".kp-btn {margin:0; font-size:calc(4vw + 4vh + 2vmin); border-radius:4mm; background-color:black; padding:2mm; color:white;}" &
         "</style><meta charset=""UTF-8""><title>Keypadder</title></head>" & ASCII.LF &
         "<body>";
      Trailer_HTML : constant String :=
         "<script>" &
         "function selTab(){var selector=document.getElementById('kpselect');openTab(selector.options[selector.selectedIndex].value);}" &
         "function openTab(tabName) { " &
         "var i; var x = document.getElementsByClassName('kp-pad');" &
         "for (i=0; i<x.length; i++) { x[i].style.display = 'none';}" &
         "document.getElementById(tabName).style.display = 'block'; } " &
         "function aget(tab, id) { " &  --  Get via AJAX
         "var form = new FormData(document.getElementById(""kpForm"")); " &
         "form.append(""tab"", tab);  form.append(""id"", id); " &
         "var data = new URLSearchParams(form).toString(); " &
         "var xhr = new XMLHttpRequest(); " &
         "xhr.open(""GET"", ""buttonpress?"" + data); " &
         "xhr.send(); return false; }" &
         "</script></form></body></html>";
      Main_HTML : Unbounded_String := Null_Unbounded_String;
      Tmp_Style : Unbounded_String := Null_Unbounded_String;
   begin
      Append (Main_HTML, Header_HTML);

      --  first the tab headers or selector
      if Conf.Keypadder_Conf.Tabswitch = Tabs then
         Append (Main_HTML, "<div class=""kp-bar"">");
         for Tab of Conf.Tabs loop
            Append (Main_HTML, "<button class=""kp-bar-item"" onclick=""openTab('" &
                                    Tab.Label & "')"">" &
                                    Tab.Label & "</button>");
         end loop;
      else -- selector
         Append (Main_HTML, "<div><select class='kp-selector' id='kpselect' onChange='selTab()'>");
         for Tab of Conf.Tabs loop
            Append (Main_HTML, "<option value='" & Tab.Label & "'>");
            Append (Main_HTML, Tab.Label & "</option>");
         end loop;
         Append (Main_HTML, "</select>");
      end if;
      Append (Main_HTML, "</div>" & ASCII.LF);

      Append (Main_HTML, "<form id=""kpForm"" onsubmit=""return ajaxget()"">" & ASCII.LF);
      --  now each tab
      for T in Conf.Tabs.First_Index .. Conf.Tabs.Last_Index loop
         Append (Main_HTML, "<div id=""" & Conf.Tabs (T).Label & """ class=""kp-pad""");
         if T /= Active_Tab then --  hide inactive Tabs
            Append (Main_HTML, " style=""display:none""");
         end if;
         Append (Main_HTML, ">" & ASCII.LF);

         --  the main content of each tab - i.e. the keys
         Append (Main_HTML, "<div style=""margin:0 auto; display:grid; gap:1rem; align-content:stretch; " &
                                 "position:fixed; top:17mm; left:0; right:0; bottom:2px; " &
                                 "overflow:scroll; " &
                                 "grid-template-columns:repeat(" & Conf.Tabs (T).Columns'Image & ", 1fr);"">");
         for K in Conf.Tabs (T).Keys.First_Index .. Conf.Tabs (T).Keys.Last_Index loop
            Append (Main_HTML, "<input type=""button"" onClick=""return aget(" & Trim (T'Image, Ada.Strings.Left) & "," &
                                    Trim (K'Image, Ada.Strings.Left) & ")"" class=""kp-btn""");
            Tmp_Style := Null_Unbounded_String;
            if Conf.Tabs (T).Keys (K).Colspan > 1 then
               Append (Tmp_Style, " grid-column: span" & Conf.Tabs (T).Keys (K).Colspan'Image & ";");
            end if;
            if Conf.Tabs (T).Keys (K).Rowspan > 1 then
               Append (Tmp_Style, " grid-row: span" & Conf.Tabs (T).Keys (K).Rowspan'Image & ";");
            end if;
            if Conf.Tabs (T).Keys (K).Bg_Colour /= Null_Unbounded_String then
               Append (Tmp_Style, " background-color: " & Conf.Tabs (T).Keys (K).Bg_Colour & ";");
            end if;
            if Conf.Tabs (T).Fontsize /= Null_Unbounded_String then
               Append (Tmp_Style, " font-size: " & Conf.Tabs (T).Fontsize & ";");
            end if;
            if Tmp_Style /= Null_Unbounded_String then
               Append (Main_HTML, " style=""" & Tmp_Style & """");
            end if;
            --  the key label
            Append (Main_HTML, " value=""" & Conf.Tabs (T).Keys (K).Label & """>" & ASCII.LF);
         end loop;
         Append (Main_HTML, "</div></div>");
      end loop;

      --  javascript to change displayed tab etc.
      Append (Main_HTML, Trailer_HTML);

      return To_String (Main_HTML);
   end Build_Main_Page;

   procedure Decode_And_Send_Key (T, I : String; Tab : out Positive) is
      Tab_Ix : constant Positive := Positive'Value (T); -- (Key_ID (Index (Key_ID, "t") + 1 .. Index (Key_ID, "i") - 1));
      Key_Ix : constant Positive := Positive'Value (I); -- (Key_ID (Index (Key_ID, "i") + 1 .. Key_ID'Last));
   begin
      --  Put_Line ("Decoded Tab:" & Tab_Ix'Image & " and Index:" & Key_Ix'Image);
      Tab := Tab_Ix;
      Injector.Injector_Task.Send (Conf.Tabs (Tab_Ix).Keys (Key_Ix).Send_Events);
   exception
      when Error : others =>
         Put_Line ("Error in Decode_And_Send_Key: ");
         Put_Line (Exception_Information (Error));
   end Decode_And_Send_Key;

end Frontend;