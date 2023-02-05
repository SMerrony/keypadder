--  SPDX-License-Identifier: GPL-3.0-or-later
--  SPDX-FileCopyrightText:  Copyright 2023 Stephen Merrony

with Config; use Config;

package Injector is

   task Injector_Task is
      entry Start;
      entry Shutdown;
      entry Send (Send_Events : Config.Event_Vectors.Vector);
   end Injector_Task;

   Cannot_Open, Cannot_Write : exception;

end Injector;