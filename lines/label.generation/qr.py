# pip install qrcode[pil] pyshorteners
import os
import qrcode
from qrcode.constants import ERROR_CORRECT_H
import pyshorteners

def shorten_url(url: str) -> str:
    """
    Shorten the given URL via pyshorteners (TinyURL by default).
    Falls back to the original URL on error.
    """
    try:
        shortener = pyshorteners.Shortener()
        return shortener.tinyurl.short(url)
    except Exception as e:
        print(f"Warning: URL shortening failed ({e}); using original URL.")
        return url


def get_qr_version(data: str,
                   version: int = None,
                   box_size: int = 30,
                   border: int = 0) -> int:
    """
    Generate QR object and return its version number without saving.
    """
    qr = qrcode.QRCode(
        version=version,
        error_correction=ERROR_CORRECT_H,
        box_size=box_size,
        border=border,
    )
    qr.add_data(data)
    qr.make(fit=True)
    return qr.version


def make_qr(data: str,
            filename: str,
            version: int = None,
            box_size: int = 30,
            border: int = 0) -> None:
    """
    Generate and save a QR-code image to 'qr/filename'.
    """
    qr = qrcode.QRCode(
        version=version,
        error_correction=ERROR_CORRECT_H,
        box_size=box_size,
        border=border,
    )
    qr.add_data(data)
    qr.make(fit=True)

    img = qr.make_image(fill_color="black", back_color="white")
    os.makedirs('qr', exist_ok=True)
    out_path = os.path.join('qr', filename)
    img.save(out_path)
    print(f"Saved QR code for {data} (version {qr.version}) as {filename}")


if __name__ == "__main__":
    import argparse

    parser = argparse.ArgumentParser(
        description="Generate a QR code PNG (box_size=30, border=0) choosing shortest URL encoding for lowest QR version"
    )
    parser.add_argument(
        "data",
        help="String or URL to encode"
    )
    parser.add_argument(
        "output",
        help="Output PNG filename (e.g. BDSC_39145.png)"
    )
    parser.add_argument(
        "--version",
        type=int,
        help="QR code version (1-40); omit for automatic sizing"
    )
    args = parser.parse_args()

    original_url = args.data
    short_url = shorten_url(original_url)

    # Compare QR versions
    orig_ver = get_qr_version(original_url, version=args.version)
    short_ver = get_qr_version(short_url, version=args.version)

    if short_ver < orig_ver:
        chosen_url = short_url
        print(f"Using shortened URL for lower QR version: {short_ver} < {orig_ver}")
    else:
        chosen_url = original_url
        print(f"Using original URL (QR version {orig_ver} <= {short_ver})")

    make_qr(
        data=chosen_url,
        filename=args.output,
        version=args.version
    )

