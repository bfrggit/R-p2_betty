while str = gets
	ln = str.chomp
	re_match = ln.match /^.*y\"\:([\d\+\-\.]+),.*x\"\:([\d\+\-\.]+)\}.*id\"\:\"([a-zA-Z\d]+)\".*offset\":([\d\+\-\.]+).*$/
	id = re_match[3]
	t = re_match[4]
	x = re_match[2]
	y = re_match[1]
	puts "#{id} #{t} #{x} #{y}"
end
