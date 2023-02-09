--  SPDX-License-Identifier: GPL-3.0-or-later
--  SPDX-FileCopyrightText:  Copyright 2023 Stephen Merrony

with Ada.Exceptions;        use Ada.Exceptions;
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
      Header_HTML : constant String :=
         "<!DOCTYPE html><html><head><style>" &
         "body {background-color: darkgray; color: white;}" &
         ".kp-bar-item {font-size: 10mm} " &
         ".kp-pad {align-content: stretch;} " &
         ".kp-btn {font-size: 20mm; border-radius: 4mm; background-color: black; padding: 2mm; color: white;}" &
         "</style></head>" & ASCII.LF;
      Trailer_HTML : constant String :=
         "<script> function openTab(tabName) { " &
         "var i; var x = document.getElementsByClassName('kp-pad');" &
         "for (i=0; i<x.length; i++) { x[i].style.display = 'none';}" &
         "document.getElementById(tabName).style.display = 'block'; } " &
         "function ajaxget(tab, id) { " &
         "var form = new FormData(document.getElementById(""kpForm"")); " &
         "form.append(""tab"", tab);  form.append(""id"", id); " &
         "var data = new URLSearchParams(form).toString(); " &
         "var xhr = new XMLHttpRequest(); " &
         "xhr.open(""GET"", ""buttonpress?"" + data); " &
         "xhr.send(); return false; }" &
         "</script>" &
         "</form></body></html>";
      Main_Page_HTML : Unbounded_String := Null_Unbounded_String;
   begin
      Append (Main_Page_HTML, Header_HTML);

      --  first the tab headers
      Append (Main_Page_HTML, "<body><div class=""kp-bar"">");
      for T in 1 .. Conf.Tabs_Count loop
         Append (Main_Page_HTML, "<button class=""kp-bar-item"" onclick=""openTab('" &
                                 Conf.Tabs (T).Label & "')"">" &
                                 Conf.Tabs (T).Label & "</button>" & ASCII.LF);
      end loop;
      Append (Main_Page_HTML, "</div>" & ASCII.LF);

      Append (Main_Page_HTML, "<form id=""kpForm"" onsubmit=""return ajaxget()"">" & ASCII.LF);
      --  now each tab
      for T in 1 .. Conf.Tabs_Count loop
         Append (Main_Page_HTML, "<div id=""" & Conf.Tabs (T).Label & """ class=""kp-pad""");
         if T /= Active_Tab then --  hide inactive Tabs
            Append (Main_Page_HTML, " style=""display:none""");
         end if;
         Append (Main_Page_HTML, ">" & ASCII.LF);

         --  the main content of each tab - i.e. the keys
         Append (Main_Page_HTML, "<div style=""margin: 0 auto; display: grid; gap: 1rem; align-content: stretch; height: 90vh;" &
                                 "grid-template-columns: repeat(" & Conf.Tabs (T).Columns'Image & ", 1fr);"">");
         for K in 1 .. Conf.Tabs (T).Keys_Count loop
            Append (Main_Page_HTML, "<input type=""button"" onClick=""return ajaxget(" & T'Image & "," & K'Image & ")"" class=""kp-btn""");
            if Conf.Tabs (T).Keys (K).Colspan > 1 then
               Append (Main_Page_HTML, " style=""grid-column: span" & Conf.Tabs (T).Keys (K).Colspan'Image & ";"" ");
            end if;
            if Conf.Tabs (T).Keys (K).Rowspan > 1 then
               Append (Main_Page_HTML, " style=""grid-row: span" & Conf.Tabs (T).Keys (K).Rowspan'Image & ";"" ");
            end if;
            Append (Main_Page_HTML, " value=""" & Conf.Tabs (T).Keys (K).Label & """/>");
         end loop;
         Append (Main_Page_HTML, "</div></div>");
      end loop;

      --  javascript to change displayed tab etc.
      Append (Main_Page_HTML, Trailer_HTML);

      return To_String (Main_Page_HTML);
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