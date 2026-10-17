"""Publish the rendered book from private source storage."""
import argparse
from html.parser import HTMLParser
from pathlib import Path
import shutil
from urllib.parse import unquote, urlsplit


class Links(HTMLParser):
    def __init__(self):
        super().__init__()
        self.values = []

    def handle_starttag(self, tag, attrs):
        self.values.extend(value for name, value in attrs if name in ("href", "src") and value)


def prepare(source, output):
    source, output = source.resolve(), output.resolve()
    if not (source / "index.html").is_file() or output.exists():
        raise ValueError("A rendered book and a new output directory are required")
    for path in source.rglob("*"):
        if path.is_symlink() or path.name in (".git", ".RData", ".Rhistory") or path.suffix in (".qmd", ".Rproj"):
            raise ValueError("The rendered book contains an unsupported source or metadata file")
    shutil.copytree(source, output)
    pages = list(output.rglob("*.html"))
    missing = []
    count = 0
    for page in pages:
        parser = Links()
        parser.feed(page.read_text(encoding="utf-8"))
        for value in parser.values:
            url = urlsplit(value)
            if url.scheme or url.netloc or not url.path:
                continue
            path = unquote(url.path)
            if path.startswith("/intro2tidy/"):
                target = output / path[len("/intro2tidy/"):]
            elif path.startswith("/"):
                continue
            else:
                target = page.parent / path
            target = target.resolve()
            if not target.is_relative_to(output):
                raise ValueError("A book reference points outside the publication")
            if target.is_dir():
                target = target / "index.html"
            count += 1
            if not target.is_file():
                missing.append(f"{page.relative_to(output)}: {value}")
    if missing:
        raise ValueError("Missing book references:\n" + "\n".join(sorted(set(missing))))
    print(f"Validated {len(pages)} book HTML pages and {count} local references.")


if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument("--source", required=True, type=Path)
    parser.add_argument("--output", required=True, type=Path)
    args = parser.parse_args()
    prepare(args.source, args.output)
