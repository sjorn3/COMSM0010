a = String(read("data-edit"))
a = split(a)


isMS(s) = try
    # println(s)
    # println(s)
    # return nothing
    return occursin("ms", s) ? true : nothing
    # return parse(Float64,s[1:end-2])
  catch
    return nothing
  end

isInt(s) = try
    return parse(Int64, s)
  catch
    return nothing
  end

b = filter(a) do s
  isMS(s) != nothing || isInt(s) != nothing
end

open("./this.csv", "w") do fp
  # ts = reshape(b, 2, length(b)//2)
  # map(ts) do
  for i in 1:2:length(b)
    write(fp, string(b[i], ",", b[i+1][1:end-2],"\n"))
  end
end

println("done")

# isMS(split(a)[8])
