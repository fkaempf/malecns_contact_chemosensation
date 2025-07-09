# pip install python-barcode[pillow]
import os
from barcode import Code128
from barcode.writer import ImageWriter

def make_barcode(data: str, filename: str, writer_options: dict = None) -> None:
    folder = "barcode"
    os.makedirs(folder, exist_ok=True)

    # default rendering options – no text
    opts = writer_options or {
        'write_text': False,
    }

    bc = Code128(data, writer=ImageWriter())
    out_path = os.path.join(folder, filename)
    # save will append .png automatically when using ImageWriter
    bc.save(out_path, options=opts)

    print(f"Saved barcode for {data} as {out_path}.png")


if __name__ == "__main__":
    import argparse
    parser = argparse.ArgumentParser(
        description="Generate a Code128 barcode PNG (bars only) for a given string"
    )
    parser.add_argument(
        "data",
        help="String to encode (eg stock ID or numeric code)"
    )
    parser.add_argument(
        "output",
        help="Output filename (without .png)"
    )
    parser.add_argument(
        "--no-text",
        action="store_true",
        help="(no-op) text is off by default"
    )
    args = parser.parse_args()

    # you could let --text turn it on; here --no-text simply does nothing
    writer_opts = {'write_text': False}

    make_barcode(
        data=args.data,
        filename=args.output,
        writer_options=writer_opts
    )
