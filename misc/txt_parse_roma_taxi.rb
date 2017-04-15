require 'date'

while str = gets
	ln = str.chomp.split(";")
	id = ln[0].to_i
	t = DateTime.parse(ln[1]).to_time.to_f
	c_match = ln[2].match /^POINT\(([\d\.\+\-]+)\s([\d\.\+\-]+)\)$/
	lat = c_match[1].to_f
	lon = c_match[2].to_f
	puts "#{id} #{t} #{lat} #{lon}"
end
