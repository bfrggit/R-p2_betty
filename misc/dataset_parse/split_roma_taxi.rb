prefix = ARGV[0]
epoch_0 = 1391209200

File.open(prefix + ".txt", "r") do |fin|
	day = 0
	fout = nil
	fin.each_line do |str|
		ln = str.chomp.split
		day_t = (ln[1].to_f.floor - epoch_0) / 86400 + 1
		if day_t > day
			fout.close if fout
			day = day_t
			fout = File.open(prefix + "_%02d.txt" % day, "w")
		end
		fout.puts str.chomp
	end
	fout.close
end
