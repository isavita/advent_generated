input = File.readlines('input.txt').map(&:chomp)

def supports_tls(ip)
  hypernet_sequences = ip.scan(/\[(.*?)\]/).flatten
  supernet_sequences = ip.split(/\[.*?\]/)

  abba = false

  supernet_sequences.each do |seq|
    (0..seq.length - 4).each do |i|
      if seq[i] == seq[i + 3] && seq[i + 1] == seq[i + 2] && seq[i] != seq[i + 1]
        abba = true
        break
      end
    end
  end

  hypernet_sequences.each do |seq|
    (0..seq.length - 4).each do |i|
      if seq[i] == seq[i + 3] && seq[i + 1] == seq[i + 2] && seq[i] != seq[i + 1]
        return false
      end
    end
  end

  abba
end

def supports_ssl(ip)
  hypernet_sequences = ip.scan(/\[(.*?)\]/).flatten
  supernet_sequences = ip.split(/\[.*?\]/)

  aba_list = []
  bab_list = []

  supernet_sequences.each do |seq|
    (0..seq.length - 3).each do |i|
      if seq[i] == seq[i + 2] && seq[i] != seq[i + 1]
        aba_list << seq[i, 3]
      end
    end
  end

  hypernet_sequences.each do |seq|
    (0..seq.length - 3).each do |i|
      if seq[i] == seq[i + 2] && seq[i] != seq[i + 1]
        bab_list << seq[i, 3]
      end
    end
  end

  aba_list.any? { |aba| bab_list.include?(aba[1] + aba[0] + aba[1]) }
end

tls_count = input.count { |ip| supports_tls(ip) }
ssl_count = input.count { |ip| supports_ssl(ip) }

puts tls_count
puts ssl_count