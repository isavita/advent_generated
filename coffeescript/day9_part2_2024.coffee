
fs = require 'fs'

class FileSegment
  constructor: (@id, @start, @end) ->

solve = ->
  line = fs.readFileSync('input.txt', 'utf8').trim()
  disk = []
  file_id = 0
  is_file = true

  for char in line
    length = parseInt(char)
    if is_file
      disk.push(String(file_id)) for i in [0...length]
      file_id++
    else
      disk.push('.') for i in [0...length]
    is_file = not is_file

  files = []
  curr_id = null
  start = 0

  for val, i in disk
    if val is '.'
      curr_id = null
      continue
    file_id = parseInt(val)
    if file_id isnt curr_id
      curr_id = file_id
      start = i
    if i is disk.length - 1 or (i + 1 < disk.length and disk[i + 1] isnt val)
      files.push new FileSegment(file_id, start, i)

  for file in files.reverse()
    file_len = file.end - file.start + 1
    leftmost_span = -1
    span_len = 0

    for i in [0...file.start]
      if disk[i] is '.'
        leftmost_span = i if span_len is 0
        span_len++
        if span_len is file_len
          break
      else
        span_len = 0
        leftmost_span = -1

    if leftmost_span isnt -1 and span_len is file_len
      disk[i] = '.' for i in [file.start..file.end]
      disk[leftmost_span + i] = String(file.id) for i in [0...file_len]
  
  checksum = 0
  for val, i in disk
      checksum += i * parseInt(val) if val isnt '.'
  console.log checksum
solve()
