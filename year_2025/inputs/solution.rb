# part1.rb
dial_position = 50
zero_crossings = 0

File.foreach('input_01.txt') do |line|
  direction = line[0]
  distance = line[1..-1].to_i

  if direction == 'L'
    # Use modulo for wrapping behavior. (a - b) % n
    dial_position = (dial_position - distance) % 100
  elsif direction == 'R'
    dial_position = (dial_position + distance) % 100
  end

  puts dial_position
  # Check if the final position for this instruction is 0
  zero_crossings += 1 if dial_position == 0
end

puts "Part 1 Answer: #{zero_crossings}"

