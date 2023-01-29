--  SPDX-License-Identifier: GPL-3.0-or-later
--  SPDX-FileCopyrightText: Copyright 2023 Stephen Merrony

with Ada.Strings.Unbounded;   use Ada.Strings.Unbounded;

with AWS.Response;
with AWS.Status;

package Frontend is

   Shutting_Down  : Boolean := False;
   Main_Page_HTML : Unbounded_String;

   function Request_CB (Request : AWS.Status.Data) return AWS.Response.Data;
   --  Main handler for HTTP requests.

   procedure Build_Main_Page;
   --  Transform the configuration into our web page.
   --  Must be called after our configuration is loaded.

end Frontend;