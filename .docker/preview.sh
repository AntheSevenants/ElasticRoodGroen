docker stop "roodgroen_preview"
docker run --rm -p "8788:8788" --name "roodgroen_preview" --mount "type=bind,src=.,target=/project/" anthesevenants/roodgroen:preview