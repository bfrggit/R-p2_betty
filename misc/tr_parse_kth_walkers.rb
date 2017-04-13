while str = gets
	ln = str.chomp.split
	if ln[1] == "create"
		puts "#{ln[2]} #{ln[0]} #{ln[3]} #{ln[4]} #{ln[5]}"
	elsif ln[1] == "setdest"
		puts "#{ln[2]} #{ln[7]} #{ln[3]} #{ln[4]} #{ln[5]}"
	end
end
