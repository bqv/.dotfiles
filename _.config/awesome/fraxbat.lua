local wibox = require("wibox")

-- Create fraxbat widget
fraxbat = wibox.widget.textbox()
fraxbat:set_align("right")

-- Globals used by fraxbat
fraxbat_st= nil
fraxbat_ts= nil
fraxbat_ch= nil
fraxbat_now = nil
fraxbat_est= nil

-- Function for updating fraxbat
function hook_fraxbat (tbw, bat)
   -- Battery Present?
   local fh= io.open("/sys/class/power_supply/"..bat.."/present", "r")
   if fh == nil then
      tbw:set_markup("No Bat")
      return(nil)
   end
   local stat= fh:read()
   fh:close()
   if tonumber(stat) < 1 then
      tbw:set_markup("Bat Not Present")
      return(nil)
   end

   -- Status (Charging, Full or Discharging)
   fh= io.open("/sys/class/power_supply/"..bat.."/status", "r")
   if fh == nil then
      tbw:set_markup("N/S")
      return(nil)
   end
   stat= fh:read()
   fh:close()
   if stat == 'Full' then
      tbw:set_markup("P:100%")
      return(nil)
   end
   stat= string.upper(string.sub(stat, 1, 1))
   if stat == 'D' then tag= 'b' else tag= 'i' end

   -- Remaining + Estimated (Dis)Charging Time
   local charge= ''
   fh= io.open("/sys/class/power_supply/"..bat.."/charge_full_design", "r")
   if fh ~= nil then
      local full= fh:read()
      fh:close()
      full= tonumber(full)
      if full ~= nil then
        fh= io.open("/sys/class/power_supply/"..bat.."/charge_now", "r")
        if fh ~= nil then
           local now= fh:read()
           local est= ''
           fh:close()
           if fraxbat_st == stat then
              delta= os.difftime(os.time(),fraxbat_ts)
              est= math.abs(fraxbat_ch - now)
              if delta > 30 and est > 0 then
                 est= delta/est
                 if now == fraxbat_now then
                    est= fraxbat_est
                 else
                    fraxbat_est= est
                    fraxbat_now= now
                 end
                 if stat == 'D' then
                    est= now*est
                 else
                    est= (full-now)*est
                 end
                 local h= math.floor(est/3600)
                 est= est - h*3600
                 est= string.format(',%02d:%02d',h,math.floor(est/60))
              else
                 est= ''
              end
           else
              fraxbat_st= stat
              fraxbat_ts= os.time()
              fraxbat_ch= now
              fraxbat_now= nil
              fraxbat_est= nil
           end
           charge=':<'..tag..'>'..tostring(math.ceil((100*now)/full))..'%</'..tag..'>'..est
        end
      end
   end
   if stat == 'D' then
	   tbw:set_markup("<span color='red'>" .. stat .. charge .. "</span>")
   else
	   tbw:set_markup(stat..charge)
   end
end

local mytimer = timer({ timeout = 10 })
mytimer:connect_signal("timeout", function () hook_fraxbat(fraxbat, 'BAT0') end)
mytimer:start()
