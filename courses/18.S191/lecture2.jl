### A Pluto.jl notebook ###
# v0.12.11

using Markdown
using InteractiveUtils

# ╔═╡ c0d6f912-33d2-11eb-0733-ad8ff87ceff7
M = [10i + j for i in 0:5, j in 1:4]

# ╔═╡ cdb598b4-33d2-11eb-0727-65fe72fe55d5
N = reshape(M, 3, 8)

# ╔═╡ 202d87f0-33d3-11eb-0de8-05239f3c1181
 v = vec(M)

# ╔═╡ 4f057e42-33d2-11eb-17d2-ab2134bf185b
w = @view v[1:2]

# ╔═╡ a116607c-33d2-11eb-1034-950ef747bc2f
w[1] = 4

# ╔═╡ a62cd3e8-33d2-11eb-1170-8714570475de
v

# ╔═╡ 491f8530-33d3-11eb-0a3c-41cf346b130d
v[1] = 3

# ╔═╡ 55952234-33d3-11eb-25ee-ef9f86c174c1
M

# ╔═╡ 5829860e-33d3-11eb-3603-eb582c711dac


# ╔═╡ Cell order:
# ╠═4f057e42-33d2-11eb-17d2-ab2134bf185b
# ╠═a116607c-33d2-11eb-1034-950ef747bc2f
# ╠═a62cd3e8-33d2-11eb-1170-8714570475de
# ╠═c0d6f912-33d2-11eb-0733-ad8ff87ceff7
# ╠═cdb598b4-33d2-11eb-0727-65fe72fe55d5
# ╠═202d87f0-33d3-11eb-0de8-05239f3c1181
# ╠═491f8530-33d3-11eb-0a3c-41cf346b130d
# ╠═55952234-33d3-11eb-25ee-ef9f86c174c1
# ╠═5829860e-33d3-11eb-3603-eb582c711dac
